unit APIConstants;

interface

const
  // Base URL and Headers
  BaseUrl = 'https://api.scryfall.com/';
  UserAgent = 'MTGCardFetch/1.0';
  AcceptHeader = 'application/json';
  MTGAppFolder = 'MTGCardFetch';


  // Endpoints
  EndpointCards = 'cards/';
  EndpointNamed = 'cards/named';
  EndpointSearch = 'cards/search';
  EndpointSets = 'sets/';
  EndpointBulkData = 'bulk-data';
  EndpointRandomCard = '/cards/random';
  EndPointCreatureTypes = 'catalog/creature-types';
  EndPointSymbology = 'https://api.scryfall.com/symbology';

  // Error Messages
  ErrorCardNotFound = 'Card not found. Please check your search criteria.';
  ErrorRequestFailed = 'Request failed: %s';
  ErrorNoDataForSetCode = 'No data returned for SetCode: %s';
  ErrorMissingDataKey = 'ParseSearchResult: "data" key not found in JSON response.';
  ErrorFillingCardDetails = 'Error filling card details: %s';

  // Log Messages
  LogFileName = 'application_log.txt';
  LogDataArrayCount = 'Data Array Count: %s';
  LogCacheHit = 'Cache hit for SetCode: %s';
  LogCacheHitDoubleCheck = 'Cache hit (double-check) for SetCode: %s';
  LogFetchingSetCode = 'Fetching SetCode from API: %s';
  LogSetCodeAddedToCache = 'SetCode added to cache: %s';

  // JSON Field Names
  FieldData = 'data';
  FieldHasMore = 'has_more';
  FieldNextPage = 'next_page';
  FieldTotalCards = 'total_cards';
  FieldTypeLine = 'type_line';
  FeildSymbol = 'symbol';
  FeildSVGUri = 'svg_uri';

  FieldID = 'id';
  FieldName = 'name';
  FieldManaCost = 'mana_cost';
  FieldOracleText = 'oracle_text';
  FieldKeywords = 'keywords';
  FieldSet = 'set';
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
  FeildGames = 'games';
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

  SVG_TEMPLATE =
    '<img style="display:inline-block; width:16px; height:16px; vertical-align:text-bottom; margin:0 2px;" ' +
    'src="data:image/svg+xml;base64,%s" alt="%s">';


implementation

end.