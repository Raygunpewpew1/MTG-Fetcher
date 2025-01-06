unit Template;

interface
var
HtmlTemplate: string = '''
  <!DOCTYPE html>
  <html lang="en">
  <head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />

  <style>
    /* General Body Styling */
    html, body {
      text-rendering: optimizeLegibility;
      -webkit-font-smoothing: antialiased;
      font-family: Beleren, Arial, sans-serif;
      background-color: rgb(38, 67, 97);
      color: white;
      height: 100%;
      margin: 0;
      padding: 0;
    }

    h1 {
      color: gold;
      font-size: 28px;
      text-align: center;
      margin: 20px 0;
    }

    h2 {
      margin: 0;
      padding: 5px 0;
    }

    /* Card Container */
    .card-container {
      display: flex;
      flex-direction: column;
      align-items: center;
      justify-content: center;
    }

    /* Flip Card Design */
    .flip-card, .single-card {
      position: relative;
      width: 240px;
      height: 360px;
      margin: 10px auto;
      border-radius: 10px;
      background: #222;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }

    .flip-card:hover, .single-card:hover {
      transform: scale(1.1);
      box-shadow: 0 8px 16px rgba(255, 255, 255, 0.5);
    }

    .flip-indicator {
      color: #fff;
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

    .flip-card.show-back {
      transform: rotateY(180deg);
    }

    .card-face {
      position: absolute;
      width: 100%;
      height: 100%;
      border-radius: 10px;
      overflow: hidden;
      backface-visibility: hidden;
      cursor: pointer;
    }

    .card-face img {
      width: 100%;
      height: 100%;
      object-fit: cover;
    }

    .card-face.back {
      transform: rotateY(180deg);
    }

    .single-card img {
      width: 100%;
      height: 100%;
      object-fit: cover;
      border-radius: 10px;
    }

    /* Card Details Styling */
    .card-details {
      width: 90%;
      max-width: 400px;
      margin: auto;
      padding: 10px;
      text-align: left;
      background: rgb(38, 67, 97);
      border-radius: 10px;
      border: 2px solid #444;
      box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
    }

    .card-name {
      text-align: center;
      font-size: 20px;
      font-weight: bold;
      margin-bottom: 5px;
      text-shadow: 0 2px 2px black;
    }

    .mana-cost {
      background: rgba(0, 0, 0, 0.85);
      color: #fff;
      padding: 5px 10px;
      font-size: 16px;
      border-radius: 5px;
      text-align: center;
      margin-top: 10px;
    }

    /* Type and Set Row */
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

    /* Rarity styling */
    .rarity {
      display: inline-block;
      padding: 5px;
      margin-top: 10px;
      border-radius: 5px;
      font-weight: bold;
      text-align: center;
      text-transform: capitalize;
    }

    .rarity.common {
      background-color: #A9A9A9; /* Gray */
      color: black;
    }

    .rarity.uncommon {
      background-color: #C0C0C0; /* Silver */
      color: black;
    }

    .rarity.rare {
      background-color: #FFD700; /* Gold */
      color: black;
    }

    .rarity.mythic {
      background-color: #FF8C00; /* Orange */
      color: #fff;
    }

    .oracle-text {
      margin-top: 10px;
      padding: 15px;
      font-family: "Times New Roman", serif;
      font-size: 15px;
      line-height: 1.5;
      color: #fff;
      background: linear-gradient(90deg, #444, #222);
      border: 3px solid #ffd700;
      border-radius: 8px;
      box-shadow: 0 0 15px rgba(255, 215, 0, 0.8);
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }
    .oracle-text:hover {
     transform: scale(1.05);
     box-shadow: 0 8px 16px rgba(255, 255, 255, 0.3);
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
      color: #fff;
      background: linear-gradient(90deg, #444, #222);
      border: 3px solid #ffd700;
      border-radius: 8px;
      box-shadow: 0 0 15px rgba(255, 215, 0, 0.8);
    }

    .power-toughness .label {
      font-weight: bold;
      color: #ffd700;
      margin-right: 10px;
    }

    .power-toughness .value {
      font-weight: bold;
      color: #ffd700;
    }

    .legalities-grid {
      display: grid;
      grid-template-columns: auto 1fr;
      gap: 2px; /* Reduced from 5px to 2px */
      padding: 5px; /* Reduced from 7px to 5px */
      margin-top: 5px;
      width: 100%;
      border: 1px solid #444;
      border-radius: 8px;
      background-color: rgb(38, 67, 97);
      box-sizing: border-box;
      border-collapse: collapse;
      font-size: 90%;
      line-height: 1.2;
      }

  .legalities-grid td {
  padding: 3px; /* Reduced cell padding from 5px to 3px */
  }
    .format-name {
      font-weight: bold;
      color: white;
      text-align: left;
    }

    .status {
      text-align: right;
    }

    .status span {
      display: inline-block;
      padding: 8px 12px;
      font-size: 12px;
      font-weight: bold;
      text-transform: uppercase;
      text-align: center;
      border-radius: 4px;
      min-width: 70px;
      color: #fff;
    }

    /* Status badges */
    .status .legal {
      background-color: #4CAF50;
      color: #fff !important;
    }

    .status .not-legal {
      background-color: #B0BEC5;
      color: #000;
    }

    .status .banned {
      background-color: #FF4500;
      color: #fff;
    }

    .status .restricted {
      background-color: #FFD700;
      color: #000;
    }

    .card-badges {
      margin-top: 10px;
      text-align: center;
    }

    .card-badges:empty {
      display: none;
    }

    .badge {
      display: inline-block;
      padding: 5px 10px;
      margin-right: 5px;
      border-radius: 3px;
      font-size: 12px;
      font-weight: bold;
      color: #fff;
    }

    .badge.full-art {
      background-color: #FF5722;
    }

    .badge.promo {
      background-color: #4CAF50;
    }

    .badge.reserved {
      background-color: #9C27B0;
    }

    .keywords {
      margin-top: 10px;
      font-size: 13px;
      font-style: italic;
      color: #FFD700;
    }

    .keywords:empty {
      display: none;
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

     /* Grid container for card faces */
     .card-faces-grid {
     display: grid;
     grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
     gap: 10px;
     padding: 10px;
     margin-top: 20px;
    }

    /* Individual card face block */
     .card-face-block {
     background: #444;
     border: 2px solid #555;
     border-radius: 8px;
     padding: 15px;
     box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
     transition: transform 0.3s ease, box-shadow 0.3s ease;
   }

   .card-face-block:hover {
     transform: scale(1.05);
     box-shadow: 0 8px 16px rgba(255, 255, 255, 0.3);
   }

   .card-face-block p {
     margin: 5px 0;
     font-size: 14px;
     line-height: 1.4;
   }

   .card-face-block strong {
    color: #FFD700; /* Gold for labels */
   }

    @media (max-width: 400px) {
      body {
        font-size: 90%;
      }

      h1 {
        font-size: 22px;
      }

      .card-container {
        padding: 10px;
      }

      .flip-card, .single-card {
        width: 100%;
        max-width: 300px;
        aspect-ratio: 2/3;
        margin: 0 auto;
        border-radius: 10px;
        background: #222;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.5);
      }

      .card-face, .single-card img {
        width: 100%;
        height: 100%;
        border-radius: 10px;
        object-fit: cover;
      }

      .card-face-details {
        margin: 10px 0;
        padding: 10px;
        border: 1px solid #444;
        border-radius: 8px;
        background-color: rgba(0, 0, 0, 0.7);
      }

      .card-face-details p {
        margin: 5px 0;
        font-size: 14px;
      }

      .card-details {
        width: 95%;
        padding: 10px;
      }

      .mana-cost {
        font-size: 14px;
      }

      .type-set-row {
        flex-direction: column;
        align-items: center;
        text-align: center;
      }

      .set-details {
        margin-top: 5px;
      }

      .oracle-text {
        font-size: 12px;
      }

      .prices-grid td,
      .additional-details p {
        font-size: 12px;
      }

      .legalities-grid {
        display: block;
        padding: 10px;
      }

      .prices-grid {
        display: block;
      }

      .legalities-grid {
        grid-template-columns: 1fr;
      }

      .prices-grid td {
        display: block;
        text-align: left;
        font-size: 14px;
        margin-bottom: 5px;
      }
    }

    .hidden {
      display: none;
    }
  </style>
  </head>
  <body>
  <div class="card-container">
    <h1 class="card-name">{{CardName}}</h1>

    {{FlipIndicator}}
    {{CardImages}}
    <p><strong>Artist:</strong> {{Artist}}</p>
    <div class="mana-cost">{{ManaCost}}</div>
    <div class="card-details">
      <div class="type-set-row">
        <div class="type-line">{{TypeLine}}</div>
        <div class="set-details">
          <img src="{{SetIcon}}" class="set-icon" alt="Set Icon">
          <span>{{SetName}}</span>
        </div>
      </div>
      <div class="games {{GamesClass}}">
      <strong><center>Available in:</strong> {{Games}}.</center>
      </div>
      <div class="oracle-text">{{OracleText}}</div>
      <div class="flavor-text">{{FlavorText}}</div>
      {{PowerToughness}}
      <div class="card-badges">
        {{FullArt}}
        {{Promo}}
        {{Reserved}}
      </div>
      <div class="keywords {{KeywordsClass}}">
        <strong>Keywords:</strong> {{Keywords}}
      </div>
      <div class="rarity {{RarityClass}}">{{Rarity}}</div>
      <div class="legalities">
        <h2>Legalities</h2>
        <table class="legalities-grid">
          {{Legalities}}
        </table>
      </div>
      <div class="prices">
        <h2>Prices</h2>
        <table class="prices-grid">
          <tr>
            <td>USD:</td>
            <td>${{USD}}</td>
          </tr>
          <tr>
            <td>USD (Foil):</td>
            <td>${{USD_Foil}}</td>
          </tr>
          <tr>
            <td>EUR:</td>
            <td>€{{EUR}}</td>
          </tr>
          <tr>
            <td>TIX:</td>
            <td>{{Tix}}</td>
          </tr>
        </table>
      </div>
      <div class="additional-details">
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
  JScript: string = 'document.addEventListener("contextmenu", function (e) { e.preventDefault(); });'
    + 'function flipCard() {' +
    '  const card = document.querySelector(".flip-card");' + '  if (card) {' +
    '    card.classList.toggle("show-back");' + '  }' + '}';

  SVG_TEMPLATE =
    '<img style="display:inline-block; width:16px; height:16px; vertical-align:text-bottom; margin:0 2px;" ' +
    'src="data:image/svg+xml;base64,%s" alt="%s">';


implementation

end.
