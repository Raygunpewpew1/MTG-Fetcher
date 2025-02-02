unit Template;

interface
var
HtmlTemplate: string = '''
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="initial-scale=1, minimum-scale=1, maximum-scale=5, user-scalable=yes">
  <style>
  html, body {
    text-rendering: optimizeLegibility;
    -webkit-font-smoothing: antialiased;
    font-family: Beleren, Arial, sans-serif;
    background-color: #20262F;
    color: #E0E6ED;
    height: 100%;
    margin: 0;
    padding: 0;
  }

  h1 { color: #FFD700; font-size: 20px; text-align: center; margin: 5px 0; }
  h2 { margin: 0; padding: 5px 0; }

  .card-container {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    max-width: 800px;
    margin: 0 auto;
    padding: 15px;
  }

  .flip-card, .single-card {
    position: relative;
    width: 240px;
    height: 335px;
    margin: 10px auto;
    border-radius: 10px;
    background: #2A2F38;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
    transition: transform 0.3s ease, box-shadow 0.3s ease;
  }

  .flip-card:hover, .single-card:hover {
    transform: scale(1.5);
    box-shadow: 0 0 8px rgba(255, 215, 0, 0.5);
  }


  .flip-indicator {
    color: #E0E6ED;
    background-color: #333;
    text-align: center;
    padding: 5px;
    font-size: 14px;
    font-weight: bold;
    margin-bottom: 10px;
    border-radius: 5px;
  }

  .flip-card {
    perspective: 1000px;
    transform-style: preserve-3d;
    transition: transform 0.8s ease;
    transform-origin: center;
  }

  .flip-card.show-back { transform: rotateY(180deg); }

  .card-face {
    position: absolute;
    width: 100%;
    height: 100%;
    border-radius: 10px;
    overflow: hidden;
    backface-visibility: hidden;
    cursor: pointer;
  }

  .card-face img, .single-card img {
    width: 100%;
    height: 100%;
    object-fit: cover;
    border-radius: 10px;
  }

  .card-face.back { transform: rotateY(180deg); }

  .card-details {
    width: 90%;
    max-width: 400px;
    margin: 15px auto;
    padding: 15px;
    text-align: left;
    background: #1A1F27;
    border-radius: 10px;
    border: 2px solid #444;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
  }

  .meld-container {
  border: 2px solid #FFD700;
  background: #1A1F27;
  border-radius: 8px;
  padding: 15px;
  margin-top: 15px;
  }

  .meld-container h2 {
  color: #FFD700;
  font-size: 18px;
  text-align: center;
  }

  .meld-parts {
  display: flex;
  flex-wrap: wrap;
  gap: 10px;
  justify-content: center;
  }

  .meld-part {
  width: 100px;
  text-align: center;
  }

  .meld-part img {
  width: 100%;
  height: auto;
  border-radius: 5px;
  }

  .meld-result {
  margin-top: 10px;
  text-align: center;
  font-size: 14px;
  color: #FFD700;
  }

  .meld-result img {
  width: 100%;
  height: auto;
  border-radius: 5px;
  }

  .meld-part img:hover{
    transform: scale(2.1);
    box-shadow: 0 0 8px rgba(255, 215, 0, 0.5);
  }

   .meld-result:hover{
    transform: scale(1.1);
    box-shadow: 0 0 8px rgba(255, 215, 0, 0.5);
  }


  .card-name {
    text-align: center;
    font-size: 20px;
    font-weight: bold;
    margin-bottom: 5px;
    text-shadow: 0 2px 2px black;
  }

  .mana-cost {
    background: #020d19;
    color: #E0E6ED;
    padding: 5px 10px;
    font-size: 16px;
    border-radius: 5px;
    text-align: center;
    margin-top: 10px;
  }

  .type-set-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin: 5px 0;
    padding: 0 10px;
  }

  .type-line {
    font-size: 14px;
    font-style: italic;
    flex-grow: 1;
    margin-right: 10px;
  }

  .set-details {
    display: flex;
    align-items: center;
    font-size: 12px;
  }

  .set-icon {
    width: 24px;
    height: 24px;
    margin-right: 5px;
    filter: invert(100%) brightness(150%);
  }

  .rarity {
    display: inline-block;
    padding: 5px;
    margin-top: 10px;
    border-radius: 5px;
    font-weight: bold;
    text-align: center;
    text-transform: capitalize;
  }

  .rarity.common { background-color: #A9A9A9; color: black; }
  .rarity.uncommon { background-color: #C0C0C0; color: black; }
  .rarity.rare { background-color: #FFD700; color: black; }
  .rarity.mythic { background-color: #FF8C00; color: #fff; }

  .oracle-text {
    margin-top: 10px;
    padding: 15px;
    font-family: "Times New Roman", serif;
    font-size: 15px;
    line-height: 1.5;
    color: #E0E6ED;
    background: linear-gradient(90deg, #444, #222);
    border: 2px solid #FFD700;
    border-radius: 8px;
    box-shadow: 0 0 8px rgba(255, 215, 0, 0.5);
  }

  .flavor-text {
    margin-top: 10px;
    padding-left: 10px;
    font-style: italic;
    color: #888;
    border-left: 2px solid rgba(255, 255, 255, 0.2);
  }

  .power-toughness {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-top: 10px;
    padding: 10px 15px;
    font-size: 16px;
    background: linear-gradient(90deg, #444, #222);
    border: 2px solid #FFD700;
    border-radius: 8px;
    box-shadow: 0 0 8px rgba(255, 215, 0, 0.5);
  }

  .power-toughness .label, .power-toughness .value {
    font-weight: bold;
    color: #FFD700;
  }

  .legalities {
  text-align: center; /* Center-align content within the legalities section */
  padding: 10px; /* Add padding around the table */
  border: 2px solid #FFD700; /* Optional: Highlight border for better visibility */
  border-radius: 10px; /* Rounded corners */
  }

  .legalities-grid {
  margin: auto; /* Center the table horizontally */
  width: 90%; /* Make the table take up 90% of the container width */
  max-width: 600px; /* Limit maximum table width */
  border-collapse: collapse; /* Remove gaps between table cells */
  }

  .legalities-grid td,
  .legalities-grid th {
  text-align: center; /* Center-align text in cells */
  padding: 8px; /* Add spacing inside cells */
  border: 1px solid #444; /* Add a border between cells */
  }

  .legalities h2 {
  margin-bottom: 10px; /* Add spacing between the header and the table */
  }
  .legalities-grid tr:hover {
  background-color: #333;
  }


  .format-name {
    font-weight: bold;
    color: #E0E6ED;
    text-align: left;
  }

  .status { text-align: right; }

  .status span {
    display: inline-block;
    padding: 8px 12px;
    font-size: 12px;
    font-weight: bold;
    text-transform: uppercase;
    text-align: center;
    border-radius: 4px;
    min-width: 70px;
  }

  .status .legal { background-color: #4CAF50; color: #fff; }
  .status .not-legal { background-color: #B0BEC5; color: #000; }
  .status .banned { background-color: #FF4500; color: #fff; }
  .status .restricted { background-color: #FFD700; color: #000; }

  .card-badges:empty { display: none; }

  .badge {
    display: inline-block;
    padding: 5px 10px;
    margin-right: 5px;
    border-radius: 3px;
    font-size: 12px;
    font-weight: bold;
    color: #fff;
  }

  .badge.full-art { background-color: #FF5722; }
  .badge.promo { background-color: #4CAF50; }
  .badge.reserved { background-color: #9C27B0; }

  .keywords {
    margin-top: 10px;
    font-size: 13px;
    font-style: italic;
    color: #FFD700;
  }

  .prices-grid {
    width: 100%;
    margin-top: 10px;
    border-collapse: collapse;
  }

  .prices-grid td {
    padding: 5px;
    border: 1px solid #444;
  }

  .prices-grid td:first-child {
    text-align: left;
    font-weight: bold;
  }

  .prices-grid td:last-child {
    text-align: right;
    color: #FFD700;
  }

  .additional-details {
    margin-top: 15px;
    font-size: 13px;
    line-height: 1.6;
    border-top: 1px solid #333;
    padding-top: 10px;
  }

  .additional-details p {
    margin: 0;
    padding: 5px 0;
  }

  button {
  background-color: #444;
  color: #FFD700;
  border: 2px solid #FFD700;
  border-radius: 5px;
  padding: 5px 10px;
  font-size: 14px;
  cursor: pointer;
  transition: background-color 0.3s;
  }

  button:hover {
  background-color: #FFD700;
  color: #20262F;
  }


  @media (max-width: 400px) {
    body { font-size: 90%; }
    h1 { font-size: 22px; }
    .card-container { padding: 10px; }

    .flip-card, .single-card {
      width: 100%;
      max-width: 300px;
      height: 420px;
    }

    .set-details { margin-top: 5px; }
    .oracle-text { font-size: 12px; }
    .prices-grid td, .additional-details p { font-size: 12px; }

    .legalities-grid, .prices-grid {
      display: block;
      padding: 10px;
    }
  }

  .hidden { display: none; }
  img {
      width: 100%;
      height: auto; /* keep aspect ratio */
    }
  </style>
  </head>
  <body>

  <!-- <h1 class="card-name">{{CardName}}</h1>
    -->


  <div class="card-details">

  <div class="card-container">
  <div class="card-name">
  {{CardName}}
  </div>
    {{FlipIndicator}}
    {{CardImages}}
     {{Rulings}}

   <div class="type-set-row">

      <div class="type-line">{{TypeLine}}</div>
      <div class="set-details">

   <div class="set-icon">{{SetIcon}}</div>

        <span>{{SetName}}</span>
      </div>
    </div>
   <div class="mana-cost">Mana Cost: {{ManaCost}}</div>
    <div class="games {{GamesClass}}">
      <strong>Available in:</strong> {{Games}}
    </div>
    <div class="rarity {{RarityClass}}">{{Rarity}}</div>
         <div class="meld-container {{MeldClass}}">
    <div class="meldclass">
    <div class="meld-parts">
    {{MeldDetails}}
    </div>
    </div>

    <div class="oracle-text">{{OracleText}}</div>
    <!--
    {{PowerToughness}}
    <div class="flavor-text">{{FlavorText}}</div>

    <div class="card-badges">
      {{FullArt}}{{Promo}}{{Reserved}}
    </div>

    <div class="keywords {{KeywordsClass}}">
      <strong>Keywords:</strong> {{Keywords}}
    </div>
    -->
        <div class="prices">
      <h2>Prices</h2>
      <table class="prices-grid">
        <tr><td>USD:</td><td>${{USD}}</td></tr>
        <tr><td>USD (Foil):</td><td>${{USD_Foil}}</td></tr>
        <tr><td>EUR:</td><td>€{{EUR}}</td></tr>
        <tr><td>TIX:</td><td>{{Tix}}</td></tr>
      </table>
    </div>

  <div class="legalities">
  <h2>Legalities</h2>
  <button onclick="toggleLegalities()" style="margin-bottom: 10px;">Toggle Legalities</button>
  <table id="legalities-grid" class="legalities-grid">
    {{Legalities}}
  </table>
  </div>


    <div class="additional-details">

      <p><strong>Artist:</strong> {{Artist}}</p>
      <p><strong>Collector Number:</strong> {{CollectorNumber}}</p>
      <p><strong>Release Date:</strong> {{ReleasedAt}}</p>
      <p><strong>Arena ID:</strong> {{Arena Id}}</p>
      <p><strong>Border Color:</strong> {{BorderColor}}</p>
      <p><strong>Story Spotlight:</strong> {{StorySpotlight}}</p>
    </div>
  </div>
  </div>
  </body>
  </html>
  ''';


  const
  JScript: string = '''
    <script>
    document.addEventListener("contextmenu", function (e) {
    e.preventDefault();
    });


    document.querySelectorAll(".flip-card").forEach(card => {
    card.addEventListener("click", function () {
        this.classList.toggle("show-back");
    });
    });

    function toggleLegalities() {
    const legalitiesGrid = document.getElementById("legalities-grid");
    if (legalitiesGrid.style.display === "none") {
      legalitiesGrid.style.display = "table"; // Show the table
    } else {
      legalitiesGrid.style.display = "none"; // Hide the table
    }
    }
    </script>
    ''';

  SVG_TEMPLATE1 =
    '<img style="display:inline-block; width:16px; height:16px; vertical-align:text-bottom; margin:0 2px;" ' +
    'src="data:image/svg+xml;base64,%s" alt="%s">';
  SVG_TEMPLATE =
  '<span style="display:inline-block; width:16px; height:16px; vertical-align:text-bottom; margin:0 2px;">%s</span>';


implementation

end.
