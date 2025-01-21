unit APIConstants;

interface

const
  // Base URL and Headers
  BaseUrl = 'https://api.scryfall.com/';
  UserAgent = 'MTGCardFetch/1.0';
  AcceptHeader = 'application/json';

  // misc
  MTGAppFolder = 'MTGCardFetch';
  FuzzySStr = '%s?fuzzy=%s';
  StandardSStr = '%s?q=%s';
  BySetCode = '+set%3A';
  ByRarity = '+rarity%3A';
  ByColor = '+c%3A';
  ShowUQ = '&unique=prints';
  SPageStr = '&page=%d';
  BulkDataURL = 'https://api.scryfall.com/bulk-data/oracle-cards';
  BulkDataPath = 'oracle-cards.json';
  MaxAutocompleteCacheEntries = 1000;
  MaxLogFileSize = 5 * 1024 * 1024;
  TrimmedLogLines = 1000;
  SetIconCacheFile = 'SetIconCache.json';
  SymbolCacheFile = 'SymbolCache.json';
  SetCacheFile = 'Sets.json';

  // Endpoints
  EndpointCards = 'cards/';
  EndpointNamed = 'cards/named';
  EndpointSearch = 'cards/search';
  EndpointSets = 'sets/';
  EndpointBulkData = 'bulk-data';
  EndpointRandomCard = '/cards/random';
  // EndPointCreatureTypes = 'catalog/creature-types';
  EndPointSymbology = 'https://api.scryfall.com/symbology';
  // dont use this in the wrapper

  // Error Messages
  ErrorCardNotFound = 'Card not found. Please check your search criteria.';
  ErrorRequestFailed = 'Request failed: %s';
  ErrorNoDataForSetCode = 'No data returned for SetCode: %s';
  ErrorMissingDataKey =
    'ParseSearchResult: "data" key not found in JSON response.';
  ErrorFillingCardDetails = 'Error filling card details: %s';

  // Log Messages
  LogFileName = 'application_log.txt';
  LogDataArrayCount = 'Data Array Count: %s';
  LogCacheHit = 'Cache hit for SetCode: %s';
  LogCacheHitDoubleCheck = 'Cache hit (double-check) for SetCode: %s';
  LogFetchingSetCode = 'Fetching SetCode from API: %s';
  LogSetCodeAddedToCache = 'SetCode added to cache: %s';

  // JSON Field Names
  FieldAllParts = 'all_parts';
  FieldEDHRank = 'edhrec_rank';
  FieldArena = 'arena_id';
  FieldData = 'data';
  FieldObject = 'object';
  FieldHasMore = 'has_more';
  FieldNextPage = 'next_page';
  FieldTotalCards = 'total_cards';
  FieldTypeLine = 'type_line';
  FieldSymbol = 'symbol';
  FieldSVGUri = 'svg_uri';
  FieldColorIdentity = 'color_identity';
  FieldID = 'id';
  FieldComponent = 'component';
  FieldName = 'name';
  FieldCount = 'total_items';
  FieldManaCost = 'mana_cost';
  FieldOracleText = 'oracle_text';
  FieldKeywords = 'keywords';
  FieldSet = 'set';
  FieldSets = 'sets';
  FieldSetName = 'set_name';
  FieldRarity = 'rarity';
  FieldPower = 'power';
  FieldToughness = 'toughness';
  FieldLoyalty = 'loyalty';
  FieldPrintsSearchUri = 'prints_search_uri';
  FieldOracleID = 'oracle_id';
  FieldFlavorText = 'flavor_text';
  FieldLayout = 'layout';
  FieldLang = 'lang';
  FieldReleasedAt = 'released_at';
  FieldCMC = 'cmc';
  FieldReserved = 'reserved';
  FieldFoil = 'foil';
  FieldNonFoil = 'nonfoil';
  FieldOversized = 'oversized';
  FieldPromo = 'promo';
  FieldReprint = 'reprint';
  FieldDigital = 'digital';
  FieldArtist = 'artist';
  FieldCollectorNumber = 'collector_number';
  FieldBorderColor = 'border_color';
  FieldFrame = 'frame';
  FieldSecurityStamp = 'security_stamp';
  FieldFullArt = 'full_art';
  FieldTextless = 'textless';
  FieldStorySpotlight = 'story_spotlight';
  FieldGames = 'games';
  FieldImageUris = 'image_uris';
  FieldSmall = 'small';
  FieldNormal = 'normal';
  FieldLarge = 'large';
  FieldPng = 'png';
  FieldBorderCrop = 'border_crop';
  FieldArtCrop = 'art_crop';
  FieldPrices = 'prices';
  FieldUsd = 'usd';
  FieldUsdFoil = 'usd_foil';
  FieldEur = 'eur';
  FieldTix = 'tix';
  FieldLegalities = 'legalities';

  // Legalities fields
  FieldStandard = 'standard';
  FieldPioneer = 'pioneer';
  FieldModern = 'modern';
  FieldLegacy = 'legacy';
  FieldCommander = 'commander';
  FieldVintage = 'vintage';
  FieldPauper = 'pauper';
  FieldHistoric = 'historic';
  FieldExplorer = 'explorer';
  FieldAlchemy = 'alchemy';
  FieldBrawl = 'brawl';
  FieldFuture = 'future';
  FieldOldschool = 'oldschool';
  FieldPremodern = 'premodern';
  FieldDuel = 'duel';
  FieldPenny = 'penny';
  FieldGladiator = 'gladiator';
  FieldOathbreaker = 'oathbreaker';
  FieldPauperCommander = 'paupercommander';
  FieldPredh = 'predh';
  FieldRelatedUris = 'related_uris';
  FieldPurchaseUris = 'purchase_uris';

  // PurchaseURIs
  FieldTcgplayer = 'tcgplayer';
  FieldCardmarket = 'cardmarket';
  FieldCardhoarder = 'cardhoarder';

  // Related URIs
  FieldGatherer = 'gatherer';
  FieldTcgplayerInfiniteArticles = 'tcgplayer_infinite_articles';
  FieldTcgplayerInfiniteDecks = 'tcgplayer_infinite_decks';
  FieldEdhrec = 'edhrec';

  // Additional fields
  FieldCardFaces = 'card_faces';
  FieldCardFaceLoyalty = 'loyalty';

  // Set Details Field Names
  FieldCode = 'code';
  FieldSetType = 'set_type';
  FieldBlock = 'block';
  FieldBlockCode = 'block_code';
  FieldParentSetCode = 'parent_set_code';
  FieldCardCount = 'card_count';
  FieldFoilOnly = 'foil_only';
  FieldIconSvgUri = 'icon_svg_uri';
  FieldScryfallUri = 'scryfall_uri';
  FieldUri = 'uri';
  FieldSearchUri = 'search_uri';

  // Cat Setup
  CatalogCreatureTypes = 'creature-types';
  CatalogPlaneswalkerTypes = 'planeswalker-types';
  CatalogArtifactTypes = 'artifact-types';
  CatalogEnchantmentTypes = 'enchantment-types';
  CatalogLandTypes = 'land-types';
  CatalogSpellTypes = 'spell-types';
  CatalogPowers = 'powers';
  CatalogToughnesses = 'toughnesses';
  CatalogLoyalties = 'loyalties';
  CatalogWatermarks = 'watermarks';
  CatalogKeywordAbilities = 'keyword-abilities';
  CatalogKeywordActions = 'keyword-actions';
  CatalogAbilityWords = 'ability-words';
  CatalogWordBank = 'word-bank';
  CatalogCardNames = 'card-names';

ColorMap:
array [0 .. 4] of record Code: string;
Name:
string;
end
= ((Code: 'W'; Name: 'White'), (Code: 'U'; Name: 'Blue'), (Code: 'B';
  Name: 'Black'), (Code: 'R'; Name: 'Red'), (Code: 'G'; Name: 'Green'));

SAboutBlank = 'about:blank';
SCatalogsJson = 'catalogs.json';
TemplateFileName = 'card_template.html';
DatabaseFileName = 'Collection.db'; //

// strings generated by CnPack

var
  S_LOADED_DATABASE_SUCCESSFULLY: string = 'Loaded database successfully!';
  S_ERROR: string = 'Error: ';
  S_FETCHING_BULK_DATA_METADATA: string = 'Fetching bulk data metadata...';
  S_DATA: string = 'data';
  S_TYPE: string = 'type';
  S_ORACLE_CARDS: string = 'oracle_cards';
  S_DOWNLOAD_URI: string = 'download_uri';
  S_DOWNLOAD_URI_FOR_DEFAULT_CARDS_NOT_FOUND
    : string = 'Download URI for "default_cards" not found.';
  S_DOWNLOADING_BULK_DATA: string = 'Downloading bulk data...';
  S_DOWNLOAD_COMPLETE_FILE_SAVED_TO
    : string = 'Download complete! File saved to: ';
  S_ERROR_DOWNLOADING_BULK_DATA: string = 'Error downloading bulk data.';
  S_ALL_SETS: string = 'All Sets';
  S_ABOUT_BLANK: string = '<html><body>Loading...</body></html>';
  S_NO_CARD_DETAILS_ARE_AVAILABLE_FOR_THIS
    : string = 'No card details are available for this item.';
  S_ERROR_FETCHING_RANDOM_CARD: string = 'Error fetching random card: ';
  S: string = ' - ';
  S_ALL_RARITIES: string = 'All Rarities';
  S_MYTHIC_RARE: string = 'Mythic Rare';
  S_MYTHIC: string = 'Mythic';
  S_ALL_COLORS: string = 'All Colors';
  S_ERROR_SEARCHING_CARDS: string = 'Error searching cards: ';
  S_SKIPPING_INVALID_CARD_AT_INDEX_D_S
    : string = 'Skipping invalid card at index %d: %s';
  S_CARDS_FOUND: string = 'Cards Found: ';
  S_SKIPPING_CARD_MISSING_NAME_OR_ID
    : string = 'Skipping card: missing "name" or "id".';
  S_ERROR_DISPLAYING_CARD: string = 'Error displaying card: ';
  S_COMMON: string = 'Common';
  S_UNCOMMON: string = 'Uncommon';
  S_RARE: string = 'Rare';
  S_SPECIAL: string = 'Special';
  S_NO_MORE_PAGES_TO_LOAD: string = 'No more pages to load.';

implementation

end.
