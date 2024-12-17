# MTG Fetcher

MTG Fetcher is a FireMonkey-based Delphi application designed to streamline Magic: The Gathering (MTG) card browsing and information retrieval using the Scryfall API. It provides a user-friendly interface for searching, viewing, and filtering card details, such as card images, legalities, prices, and more.

---

## About
It needs a lot of work!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

This project was developed using **Delphi Community Edition**, a free version of Embarcadero Delphi intended for individual developers, open-source projects, and small companies. It leverages Delphi’s powerful FireMonkey (FMX) framework to create a cross-platform application with support for Windows and Android. 

The application retrieves MTG card data directly from Scryfall, enabling advanced features such as:
- Card image viewing (including support for double-faced cards).
- Filtering cards by set, rarity, color, and other attributes.
- Displaying card legalities, power/toughness, mana cost, and prices.
- A visually rich, responsive UI.

---

## Dependencies

This project uses the following third-party library:
- **JsonDataObjects**: A fast and lightweight JSON parser for Delphi. See the official repository here: [JsonDataObjects on GitHub](https://github.com/ahausladen/JsonDataObjects).

The **JsonDataObjects** library is licensed under the MIT License.

---

## Installation
Installation Instructions

To set up and run the application on another machine, follow these steps:

1. Build and Run the Application:

Open the project in Delphi and build it.

Run the application once to generate the necessary files and folder structure.

This will create a folder in your AppData directory on Windows:
\Users\username\AppData\Roaming\MTGCardFetch



2. Verify Setup:

Ensure that the catalogs.json file is present in the \Users\username\AppData\Roaming\MTGCardFetch folder after running the app for the first time.

Double-check that the card_template.html file is the same directory.


3. Run the Application:

The application should now work as intended!





### Requirements
1. Delphi Community Edition (or any other edition) is required to build this application.
   - Download it here: [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).
2. JsonDataObjects must be included in your Delphi environment.

### Steps
1. Clone this repository to your local machine.
2. Open the project in Delphi.
3. Compile and run the application on your desired platform (Windows or Android).

---

## License

All images are copyright Wizards of the Coast ([http://magicthegathering.com](http://magicthegathering.com))

This project is licensed under the MIT License. See the [LICENSE](./LICENSE) file for more details.

**Note:** This project was created using Delphi Community Edition, which is free for certain use cases, including open-source projects and individual developers. Ensure your use of Delphi complies with the Community Edition license terms.

The **JsonDataObjects** library, used in this project, is also licensed under the MIT License. You can find details in its respective repository.

---
  
This project is powered by the Scryfall API but is not produced or endorsed by Scryfall.  
For more information, visit: https://scryfall.com/


## Acknowledgments

- **Scryfall**: This project uses the Scryfall API to retrieve MTG card data. [Scryfall API documentation](https://scryfall.com/docs/api).
- **JsonDataObjects**: For providing a robust JSON-handling library for Delphi.
- **Delphi Community Edition**: For offering a powerful, accessible development tool for individual developers and small teams.
