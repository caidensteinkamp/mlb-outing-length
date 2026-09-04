# =============================================================================
# MLB Starters -- Outing Length Leaderboard
#
# Reads the bundle written by mlb_outing_length.R and ranks starting pitchers on
# how well they generate long outings.
#
#   shiny::runApp("mlb_outing_length_app.R")      # from the repo directory
#
# This is the Butler outing-length app pointed at Baseball Savant and turned
# around. The Butler version was PRESCRIPTIVE -- it took a staff we control and
# told each arm where to aim, which pitch to throw more, what to build in the
# bullpen. None of that applies to 200 major leaguers we don't employ, so the
# aim / shape / command tabs are gone. What replaces them is a ranking layer.
#
# Two things carried over unchanged, because they are the actual content:
#
#   1. The pitch economy premise. An outing is a pitch budget, a ground-ball out
#      costs ~3.3 pitches and a strikeout ~4.9 for the same one out, and a walk
#      costs ~10 because you still owe the out. See the "Why this works" tab.
#
#   2. The two-axis rule. Outing length ALONE always prefers pitching to
#      contact, so no pitcher is called good on OLV without runs saved beside
#      it. Ranking on outing length by itself would crown soft-tossing
#      contact-managers, which is exactly the failure mode the Butler build had
#      to design around.
#
# The headline column is xOuts/start, and it is the one number here that is not
# a rescaling of something already on a public leaderboard. See the Leaderboard
# tab's notes for what it does and does not remove.
# =============================================================================

library(shiny)
library(data.table)
library(ggplot2)
library(DT)
library(ggrepel)

# Bundle resolution, in priority order:
#   1. OL_BUNDLE env var  -- view a specific bundle, e.g.
#      OL_BUNDLE=mlb_outing_bundle.rds Rscript -e 'shiny::runApp(...)'
#   2. data/bundle.rds    -- how a DEPLOYED copy finds its data. On
#      shinyapps.io the working directory is the app directory, so the slim
#      bundle written by make_deploy.R sits right here beside app.R.
#   3. a bundle next to this file, 2026-only first, then the pooled one.
#
# Nothing here is an absolute path. A hardcoded ~/Downloads path used to sit at
# the end of this chain, which worked but made rsconnect warn on every deploy
# that the project references files outside itself -- fair, since that path
# exists on exactly one machine and never on the server.
BUNDLE <- local({
  env <- Sys.getenv("OL_BUNDLE")
  if (nzchar(env)) return(env)
  if (file.exists(file.path("data", "bundle.rds"))) return(file.path("data", "bundle.rds"))
  here <- local({
    a <- commandArgs(FALSE)
    f <- sub("^--file=", "", a[grep("^--file=", a)])
    if (length(f)) normalizePath(dirname(f[1])) else getwd()
  })
  for (nm in c("mlb_outing_bundle_2026.rds", "mlb_outing_bundle.rds")) {
    p <- file.path(here, nm)
    if (file.exists(p)) return(p)
  }
  file.path(here, "mlb_outing_bundle.rds")   # reported by the check below
})

if (!file.exists(BUNDLE))
  stop("Bundle not found. Run mlb_outing_length.R first to create:\n  ", BUNDLE)

B <- readRDS(BUNDLE)

board   <- as.data.table(B$board)
outings <- as.data.table(B$outings)
mix     <- as.data.table(B$mix)
prof    <- as.data.table(B$prof)
xo      <- as.data.table(B$xo)
V       <- B$V_lg
LG_X    <- B$lg_xouts
SEASONS <- paste(B$meta$seasons, collapse = " + ")

# board already carries the xOuts decomposition columns (gain_count, gain_bip)
# from the model script -- merging xo in again here silently produced .x/.y
# duplicates and orphaned every reference to them.
stopifnot(all(c("gain_count", "gain_bip", "name") %in% names(board)))
setorder(board, -xouts)

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("MLB Starters -- Outing Length Leaderboard"),
  tags$p(style = "color:#555;margin-top:-8px",
         sprintf("Baseball Savant pitch-level data, %s regular season. ", SEASONS),
         "The headline metric is ", tags$b("xOuts/start"),
         ": expected outs recorded per start using each pitcher's own ",
         "pitch-level event rates, under a league-average manager. ",
         sprintf("The league baseline is %.2f outs (%.2f IP).", LG_X, LG_X / 3)),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      sliderInput("min_gs", "Minimum starts", min = B$meta$MIN_STARTS,
                  max = max(board$starts), value = B$meta$MIN_STARTS, step = 1),
      checkboxGroupInput("hand", "Throws", choices = c("R", "L"),
                         selected = c("R", "L"), inline = TRUE),
      hr(),
      selectInput("pitcher", "Pitcher card", choices = board$name,
                  selected = board$name[1]),
      hr(),
      htmlOutput("scorecard")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Leaderboard",
          tags$p(style = "color:#555",
            tags$b("Why not just rank innings per start?"), " Because that is ",
            "mostly a measure of the manager. Leash length, bullpen quality, ",
            "roster construction and September shutdowns all move it, and none ",
            "of them are the pitcher. ", tags$b("xOuts/start"), " instead takes ",
            "each pitcher's own per-pitch event rates -- how often he throws a ",
            "ball, a called or swinging strike, a foul, or puts it in play from ",
            "each count -- plus his own outs-per-ball-in-play, and solves the ",
            "outing-length dynamic program with the ",
            tags$i("league-average removal hazard"), ". Holding the manager ",
            "constant is the whole trick."),
          tags$p(style = "color:#555",
            tags$b("What it does not remove:"), " balls in play are credited as ",
            "the outs they actually produced, so a starter in front of a good ",
            "infield really does bank cheaper outs. That is genuinely part of ",
            "his outing length without being his skill, which is why the ",
            tags$b("Sources"), " tab splits each pitcher's edge into a count ",
            "channel (command and whiffs -- his) and a contact channel ",
            "(contact quality, defense, park -- partly not his)."),
          tags$p(style = "color:#555",
            tags$b("Leash"), " = actual outs minus xOuts: positive means he was ",
            "allowed to go deeper than his stuff earned, negative means he was ",
            "pulled early relative to what he earned."),
          DTOutput("t_board")),

        tabPanel("Two axes",
          tags$p(style = "color:#555",
            "Outing length on its own is not a virtue. The cheapest out in ",
            "baseball is a ground ball, so a pitcher who lets everything get ",
            "put in play looks maximally efficient right up until you notice ",
            "the runs -- which is why nothing in this app ranks on outing ",
            "length alone. The pitchers who matter are ", tags$b("upper right"),
            ": efficient with a pitch budget ", tags$i("and"), " preventing ",
            "runs while they spend it."),
          plotOutput("p_quad", height = "620px"),
          br(), h4("Best on both axes"), DTOutput("t_both")),

        tabPanel("Sources",
          tags$p(style = "color:#555",
            "Where each pitcher's expected outing length comes from. Both bars ",
            "are outs per start above or below the league baseline, and they ",
            "are computed by swapping one channel to league rates while holding ",
            "the other at his own."),
          tags$p(style = "color:#555",
            tags$b("Count channel"), " -- his ball / strike / foul / in-play ",
            "rates by count. Command and bat-missing. This is his. ",
            tags$b("Contact channel"), " -- how many outs a ball in play off ",
            "him converts into. Partly contact quality, partly the defense and ",
            "park behind him. Treat a pitcher whose whole edge is the contact ",
            "channel with more caution than one whose edge is in the count."),
          plotOutput("p_sources", height = "640px")),

        tabPanel("Pitcher card",
          h4(textOutput("card_title")),
          plotOutput("p_card", height = "300px"),
          br(), h4("Pitch mix"),
          tags$p(style = "color:#555",
            "OLV/100 is outs of outing per 100 pitches of this pitch type; ",
            "RS/100 is runs saved per 100. A pitch can be good at one and bad ",
            "at the other -- that is the trade, not an error."),
          DTOutput("t_mix"),
          br(), h4("Outing distribution"), plotOutput("p_hist", height = "260px")),

        tabPanel("Leash",
          tags$p(style = "color:#555",
            "Actual outs per start against expected. The 45-degree line is a ",
            "manager who uses a starter exactly as deep as his stuff earns. ",
            "Above it is a long leash, below it a short one. Read this as a ",
            "usage finding, not a pitcher finding -- a point well below the ",
            "line is a club decision (a piggyback, an innings cap, a bullpen ",
            "game, an injury return), not a flaw in the arm."),
          plotOutput("p_leash", height = "600px"),
          br(), fluidRow(
            column(6, h4("Longest leash"), DTOutput("t_long")),
            column(6, h4("Shortest leash"), DTOutput("t_short")))),

        tabPanel("Why this works",
          tags$p(style = "color:#555",
            "The premise behind every column, and the one part of this app that ",
            "is the same for every pitcher. A ground-ball out and a strikeout ",
            "are both one out, but they cost very different numbers of pitches, ",
            "and an outing is a pitch budget."),
          plotOutput("p_outcome", height = "400px"),
          tags$p(style = "color:#555",
            "Note the strikeout. It sits barely above zero on outing length ",
            "while a ground-ball out is five times better, and a double play is ",
            "the single most valuable event in baseball for staying in a game. ",
            "But look at the runs column in the table below: the strikeout is ",
            "as good as a ground-ball out at preventing runs. That is the ",
            "entire reason this app never ranks on one axis."),
          plotOutput("p_ole", height = "300px"),
          h4("Pitch economy of each outcome"),
          tags$p(style = "color:#555",
            "Dividing a plate appearance's own pitches by the outs it recorded ",
            "is useless for anything that doesn't make an out -- a walk would ",
            "come out at infinity. It didn't cost infinity; it cost ",
            tags$b("the pitches spent getting the out it failed to produce"),
            ". So ", tags$code("pitches_to_out"), " walks forward from the ",
            "start of each plate appearance until an out is actually banked. ",
            "The windows overlap by construction, so the column is a ",
            "conditional waiting time and cannot be summed to an outing total."),
          DTOutput("t_econ"))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  filt <- reactive({
    b <- board[starts >= input$min_gs & p_throws %in% input$hand]
    setorder(b, -xouts)
    b[, rank_x := seq_len(.N)]
    b
  })

  # Keep the pitcher-card dropdown in sync with the filters.
  #
  # The NULL guard is load-bearing. `input$pitcher` is unset until the client
  # sends its first value, and `if (NULL %in% b$name)` is a zero-length
  # condition, which is an ERROR in R, not FALSE. That error kills the observer
  # and poisons the whole session, so every output on the page fails at once --
  # which is exactly how it looked when the headless smoke test drove the app
  # without ever setting a pitcher.
  observe({
    b <- filt()
    if (!nrow(b)) return()
    sel <- if (!is.null(input$pitcher) && input$pitcher %in% b$name)
      input$pitcher else b$name[1]
    updateSelectInput(session, "pitcher", choices = b$name, selected = sel)
  })

  cur <- reactive({
    b <- filt()
    # A hand + minimum-starts combination can legitimately select nobody, and
    # b[1] on an empty table yields a row of NAs that renders as "NA (NAHP)"
    # rather than failing, so stop here with something readable instead.
    validate(need(nrow(b), "No pitchers match these filters."))
    if (is.null(input$pitcher)) return(b[1])
    r <- b[name == input$pitcher]
    if (!nrow(r)) b[1] else r
  })

  BOARD_COLS <- function(d) d[, .(
    Rank = rank_x, Pitcher = name, Thr = p_throws, GS = starts,
    `xOuts/st` = round(xouts, 2), `xIP/st` = round(xouts / 3, 2),
    `Act outs` = round(outs, 2), `Act IP` = round(ip, 2),
    Leash = round(leash, 2), `P/st` = round(pitches, 1),
    `P/out` = round(p_per_out, 2), `OLV/100` = round(olv100, 2),
    `RS/100` = round(rv100, 2), ERA = round(era, 2),
    `6+IP%` = round(100 * six_plus), `7+IP%` = round(100 * seven_plus))]

  output$scorecard <- renderUI({
    r <- cur(); b <- filt()
    HTML(sprintf(
      "<b>%s</b> (%sHP)<br>%d starts<br><br>
       xOuts/start: <b>%.2f</b> <span style='color:#777'>(%.2f IP)</span><br>
       Rank: <b>%d of %d</b><br>
       League baseline: %.2f<br><br>
       Actual: <b>%.2f outs</b> (%.2f IP)<br>
       Leash: <b>%+.2f</b><br><br>
       OLV/100: <b>%+.2f</b><br>Runs saved/100: <b>%+.2f</b><br>
       Pitches per out: <b>%.2f</b><br><br>
       <span style='color:#777'>Edge from count channel %+.2f<br>
       Edge from contact channel %+.2f</span>",
      r$name, r$p_throws, r$starts, r$xouts, r$xouts / 3, r$rank_x, nrow(b),
      LG_X, r$outs, r$ip, r$leash, r$olv100, r$rv100, r$p_per_out,
      r$gain_count, r$gain_bip))
  })

  output$t_board <- renderDT({
    b <- filt()
    validate(need(nrow(b), "No pitchers match these filters."))
    datatable(BOARD_COLS(b), rownames = FALSE,
              options = list(pageLength = 25, order = list()),
              caption = "Sorted by xOuts/start. Click any header to re-sort.") |>
      formatStyle("xOuts/st", fontWeight = "bold")
  })

  # ---- two axes -------------------------------------------------------------
  output$p_quad <- renderPlot({
    b <- copy(filt())
    validate(need(nrow(b), "No pitchers match these filters."))
    mx <- median(b$xouts); my <- median(b$rv100)
    # Label the extremes rather than all ~200 points: the interesting names are
    # the corners, and a fully labelled scatter is unreadable. Note the `b$` --
    # these run outside a data.table `[` so bare column names are not in scope.
    both <- rank(b$xouts) + rank(b$rv100)   # good/bad at both
    trade <- rank(b$xouts) - rank(b$rv100)  # good at one, bad at the other
    b[, lab := ""]
    b[unique(c(head(order(-both), 12), head(order(both), 5),
               head(order(-trade), 4), head(order(trade), 4))), lab := name]

    ggplot(b, aes(xouts, rv100)) +
      annotate("rect", xmin = mx, xmax = Inf, ymin = my, ymax = Inf,
               fill = "steelblue", alpha = .07) +
      geom_hline(yintercept = my, colour = "grey60", linetype = "dashed") +
      geom_vline(xintercept = mx, colour = "grey60", linetype = "dashed") +
      geom_vline(xintercept = LG_X, colour = "firebrick", linewidth = .4) +
      geom_point(aes(size = starts), colour = "grey35", alpha = .55) +
      geom_text_repel(aes(label = lab), size = 3.4, max.overlaps = 40,
                      min.segment.length = 0, seed = 1) +
      scale_size_continuous(range = c(1.2, 4.5), guide = "none") +
      annotate("text", x = Inf, y = Inf, label = "efficient AND effective",
               hjust = 1.05, vjust = 1.6, size = 4, colour = "steelblue4",
               fontface = "bold") +
      labs(title = "Outing length against run prevention",
           subtitle = paste("Dashed lines are medians; the red line is the",
                            "league-average expected outing.\nPoint size is",
                            "starts. Ranking on the x-axis alone would reward",
                            "pitching to contact, which is why it isn't done."),
           x = "xOuts per start (expected outing length)",
           y = "Runs saved per 100 pitches") +
      theme_minimal(base_size = 13)
  })

  output$t_both <- renderDT({
    b <- copy(filt())
    validate(need(nrow(b), "No pitchers match these filters."))
    b <- b[xouts >= median(xouts) & rv100 >= median(rv100)]
    # setorder() takes column names, not expressions -- the combined rank has to
    # be materialised as a column first.
    b[, both := rank(xouts) + rank(rv100)]
    setorder(b, -both)
    datatable(BOARD_COLS(head(b, 20)), rownames = FALSE,
              options = list(dom = "t", pageLength = 20))
  })

  # ---- sources --------------------------------------------------------------
  output$p_sources <- renderPlot({
    b <- head(filt()[order(-xouts)], 25)
    validate(need(nrow(b), "No pitchers match these filters."))
    d <- rbind(
      data.table(name = b$name, channel = "Count (command, whiffs)",
                 val = b$gain_count),
      data.table(name = b$name, channel = "Contact (quality, defense, park)",
                 val = b$gain_bip))
    d[, name := factor(name, levels = rev(b$name))]
    ggplot(d, aes(val, name, fill = channel)) +
      geom_col(position = "stack", width = .72) +
      geom_vline(xintercept = 0) +
      scale_fill_manual(values = c("Count (command, whiffs)" = "steelblue4",
                                   "Contact (quality, defense, park)" = "goldenrod2")) +
      labs(title = "Where expected outing length comes from -- top 25",
           subtitle = paste("Outs per start above the league baseline, by",
                            "channel. Each is computed by swapping the OTHER\n",
                            "channel to league rates, so the two are not",
                            "constrained to sum exactly to the total."),
           x = "Outs per start vs league baseline", y = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  # ---- pitcher card ---------------------------------------------------------
  output$card_title <- renderText({
    r <- cur()
    sprintf("%s -- %.2f xOuts/start (%.2f IP), rank %d",
            r$name, r$xouts, r$xouts / 3, r$rank_x)
  })

  output$p_card <- renderPlot({
    r <- cur()
    p <- prof[pitcher == r$pitcher]
    validate(need(nrow(p), "No profile rows for this pitcher."))
    lg <- B$lg_prof
    d <- data.table(
      metric = factor(c("Ball", "Strike", "Foul", "In play"),
                      levels = c("Ball", "Strike", "Foul", "In play")),
      him = 100 * c(p$ball, p$strike, p$foul, p$inplay),
      lgv = 100 * c(lg$ball, lg$strike, lg$foul, lg$inplay))
    ggplot(d, aes(him, metric)) +
      geom_segment(aes(x = lgv, xend = him, yend = metric),
                   colour = "grey70", linewidth = 1.2) +
      geom_point(aes(x = lgv), size = 3.6, shape = 21, fill = "white",
                 colour = "grey35") +
      geom_point(size = 4.4, colour = "steelblue4") +
      geom_text(aes(label = sprintf("%+.1f", him - lgv)), vjust = -1.2,
                size = 3.4, colour = "steelblue4") +
      labs(title = "Per-pitch event rates vs league",
           subtitle = paste("Blue is him, open circle is league. These are the",
                            "rates that drive his xOuts.\nFewer balls and more",
                            "cheap contact both buy outing length; strikeouts",
                            "cost pitches."),
           x = "% of his pitches", y = NULL) +
      theme_minimal(base_size = 13)
  })

  output$t_mix <- renderDT({
    r <- cur()
    m <- mix[pitcher == r$pitcher]
    validate(need(nrow(m), "No pitch type reaches the 25-pitch floor."))
    setorder(m, -usage)
    datatable(m[, .(Pitch = pitch_name, N = n,
                    `Usage %` = round(100 * usage),
                    Velo = round(velo, 1),
                    `Ball %` = round(100 * ball), `Strike %` = round(100 * strike),
                    `Foul %` = round(100 * foul), `In play %` = round(100 * inplay),
                    `OLV/100` = round(olv100, 2), `RS/100` = round(rv100, 2))],
              rownames = FALSE, options = list(dom = "t", pageLength = 10))
  })

  output$p_hist <- renderPlot({
    r <- cur()
    o <- outings[pitcher == r$pitcher]
    ggplot(o, aes(outs)) +
      geom_histogram(binwidth = 1, fill = "steelblue4", colour = "white") +
      geom_vline(xintercept = r$xouts, colour = "firebrick", linewidth = 1) +
      geom_vline(xintercept = mean(o$outs), colour = "grey30",
                 linetype = "dashed", linewidth = .8) +
      labs(title = "Outs recorded per start",
           subtitle = paste("Red is his xOuts (what he earned);",
                            "dashed grey is his actual mean (what he was allowed)."),
           x = "Outs recorded", y = "Starts") +
      theme_minimal(base_size = 13)
  })

  # ---- leash ----------------------------------------------------------------
  output$p_leash <- renderPlot({
    b <- copy(filt())
    validate(need(nrow(b), "No pitchers match these filters."))
    b[, lab := ""]
    b[head(order(-leash), 8), lab := name]
    b[head(order(leash), 8), lab := name]
    ggplot(b, aes(xouts, outs)) +
      geom_abline(slope = 1, intercept = 0, colour = "firebrick", linewidth = .6) +
      geom_point(aes(size = starts), colour = "grey35", alpha = .55) +
      geom_text_repel(aes(label = lab), size = 3.4, max.overlaps = 40,
                      min.segment.length = 0, seed = 2) +
      scale_size_continuous(range = c(1.2, 4.5), guide = "none") +
      labs(title = "Allowed against earned",
           subtitle = paste("The red line is a manager who uses a starter",
                            "exactly as deep as his stuff earns.\nAbove it is",
                            "a long leash, below it a short one."),
           x = "xOuts per start (earned)", y = "Actual outs per start (allowed)") +
      theme_minimal(base_size = 13)
  })

  LEASH_COLS <- function(d) d[, .(
    Pitcher = name, GS = starts, `xOuts/st` = round(xouts, 2),
    `Act outs` = round(outs, 2), Leash = round(leash, 2),
    `P/st` = round(pitches, 1), ERA = round(era, 2))]

  output$t_long <- renderDT({
    b <- filt()
    validate(need(nrow(b), "No pitchers match these filters."))
    datatable(LEASH_COLS(head(b[order(-leash)], 12)), rownames = FALSE,
              options = list(dom = "t", pageLength = 12))
  })

  output$t_short <- renderDT({
    b <- filt()
    validate(need(nrow(b), "No pitchers match these filters."))
    datatable(LEASH_COLS(head(b[order(leash)], 12)), rownames = FALSE,
              options = list(dom = "t", pageLength = 12))
  })

  # ---- why this works -------------------------------------------------------
  output$p_outcome <- renderPlot({
    d <- as.data.table(B$outcome_value)
    d[, outcome := factor(outcome, levels = d[order(olv), outcome])]
    ggplot(d, aes(olv, outcome, fill = olv > 0)) +
      geom_col(width = .7, show.legend = FALSE) +
      geom_vline(xintercept = 0) +
      geom_text(aes(label = sprintf("%+.2f", olv),
                    hjust = ifelse(olv > 0, -0.15, 1.15)), size = 3.4) +
      scale_fill_manual(values = c(`TRUE` = "steelblue", `FALSE` = "firebrick")) +
      expand_limits(x = c(-1.3, 1.6)) +
      labs(title = "What extends an MLB start",
           subtitle = "Outing Length Value per plate appearance, in outs of outing",
           x = NULL, y = NULL) +
      theme_minimal(base_size = 13)
  })

  output$p_ole <- renderPlot({
    d <- data.table(pitches = 0:135,
                    remaining = vapply(0:135, function(p) V[p + 1, 1, 1, 1, 7], 0))
    ggplot(d, aes(pitches, remaining)) +
      geom_line(linewidth = 1) +
      labs(title = "Outing Length Expectancy",
           subtitle = paste("Expected outs still to be recorded, fresh hitter,",
                            "0 outs in the inning, 6 already banked"),
           x = "Pitches thrown", y = "Expected remaining outs") +
      theme_minimal(base_size = 13)
  })

  output$t_econ <- renderDT({
    d <- as.data.table(B$economy)
    datatable(d[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)],
              rownames = FALSE, options = list(dom = "t", pageLength = 20))
  })
}

shinyApp(ui, server)
