ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body, .container-fluid {
        height: 100%;
        margin: 0;
        padding: 0;
      }

      #main-layout {
        height: 100vh;
        display: flex;
        flex-direction: column;
      }

      #top-section {
        display: flex;
        flex-basis: 50%;
        overflow: hidden;
      }

      #left-panel {
        resize: horizontal;
        overflow: auto;
        min-width: 200px;
        max-width: 90%;
        padding: 10px;
        border: 1px solid #ccc;
      }

      #right-panel {
        flex: 1;
        overflow: auto;
        padding: 10px;
        border: 1px solid #ccc;
      }

      #drag-handle {
        height: 6px;
        background-color: #888;
        cursor: row-resize;
      }

      #bottom-section {
        flex-grow: 1;
        overflow: auto;
        padding: 10px;
        border-top: 2px solid #666;
      }
    ")),
    
    tags$script(HTML("
      $(document).ready(function () {
        // Redimensionnement vertical entre haut/bas
        let isDragging = false;

        $('#drag-handle').on('mousedown', function (e) {
          e.preventDefault();
          isDragging = true;

          let layout = $('#main-layout');
          let handle = $('#drag-handle');
          let top = $('#top-section');
          let bottom = $('#bottom-section');

          $(document).on('mousemove.drag', function (e) {
            if (!isDragging) return;

            let offset = layout.offset().top;
            let y = e.pageY - offset;
            let total = layout.height();
            let handleHeight = handle.height();

            let topHeight = y;
            let bottomHeight = total - topHeight - handleHeight;

            if (topHeight < 150 || bottomHeight < 150) return;

            top.css('flex-basis', topHeight + 'px');
            bottom.css('flex-basis', bottomHeight + 'px');
          });

          $(document).on('mouseup.drag', function () {
            isDragging = false;
            $(document).off('.drag');
          });
        });
      });
    "))
  ),
  # -------------------- Layout principal --------------------
  div(id = "main-layout",
      
      # ------ SECTION SUPÉRIEURE : gauche/droite ------
      div(id = "top-section",
          div(id = "left-panel", class = "resizable-panel",
              uiOutput("top_left")
          ),
          div(id = "right-panel", class = "resizable-panel",
              uiOutput("top_right")
          )
      ),
      
      # ------ SÉPARATEUR DRAGGABLE ------
      div(id = "drag-handle"),
      
      # ------ SECTION INFÉRIEURE : tableau + graphiques ------
      div(id = "bottom-section",
          tabsetPanel(
            id = "bottom_tabs",
            tabPanel("Tableau",
                     withSpinner(DTOutput("selected_table"))
            ),
            tabPanel("Statistiques d'abondance",
                     fluidRow(
                       column(
                         width = 12,
                         plotOutput("stat_plot_placeholder")
                       )
                     )
            ),
            tabPanel("Statistiques d'importance",
                     fluidRow(
                       column(
                         width = 12,
                         plotOutput("stat_plot_placeholder_2")
                       )
                     )
            ),
          )
      )
  )
)