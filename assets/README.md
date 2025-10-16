# Assets Directory

This directory contains all static assets used by the Hyperswitch SDK, including localization files, icons, and configuration files. These assets are synchronized to AWS S3 for efficient distribution and are served via CloudFront CDN.

## Directory Structure

```
assets/
|   v1/                          # Version 1 assets
|   |-> configs/                 # Configuration files
|   |   |-> superposition.config.json
|   | 
|   |-> icons/                   # Icon assets
|   |   |-> mobile/             # Mobile-specific icons (SVG, JPEG)
|   |   |-> web/                # Web-specific icons
|   | 
|   |-> jsons/                  # JSON data files
|   |   |-> locales/            # Locale translation files (~32 languages)
|   |   |-> location/           # Location-related data
|   | 
|   |-> jsons-gzips/            # Gzipped versions of JSON files
|       |-> locales/            # Compressed locale files
|       |-> location/           # Compressed location files
|
|-> v2/                          # Version 2 assets
    |-> configs/                 # Configuration files
    |   |  superposition.config.json
    |   | 
    |-> icons/                   # Icon assets (SVG)
    |-> jsons/                   # JSON data files
        |->  locales/            # Locale translation files
        |->  location/           # Location-related data
```

## Asset Types

### 1. Localization Files (`jsons/locales/`)
Translation files for internationalization support across ~32+ languages including:
- English (en, en-GB)
- European languages (de, fr, es, it, pt, nl, etc.)
- Asian languages (zh, ja, ar, he, etc.)
- Nordic languages (sv, da, no, fi)
- And many more

**Format**: JSON files containing key-value pairs for UI strings
**Example structure**:
```json
{
  "locale": "en",
  "localeDirection": "ltr",
  "cardNumberLabel": "Card Number",
  "cardDetailsLabel": "Card Details",
  ...
}
```

### 2. Icons (`icons/`)
SVG and image assets for payment methods, UI elements, and branding:
- **Payment methods**: Visa, Mastercard, American Express, PayPal, Apple Pay, Google Pay, etc.
- **UI elements**: Cards, locks, arrows, checkboxes, etc.
- **Platform-specific**: Separate icons for mobile and web

**Formats**: `.svg` (primary), `.jpeg`, `.gif`

### 3. Configuration Files (`configs/`)
- **superposition.config.json**: Dynamic configuration for payment method handling per connector
  - Defines context-based overrides
  - Maps payment methods to connectors (Stripe, Adyen, PayPal, etc.)
  - Configures regional payment method support

### 4. Location Data (`jsons/location/`)
Country, state, and regional information for address forms and validation.

### 5. Compressed Assets (`jsons-gzips/`)
Gzipped versions of JSON files for optimized network transfer. The SDK automatically requests these compressed versions when supported by the client.

## AWS S3 Synchronization

### Overview
All assets in this directory are synchronized to AWS S3 and distributed via CloudFront CDN for global availability and optimal performance. The SDK fetches assets from S3 at runtime rather than bundling them in the application.

### S3 Bucket Structure
Assets are organized in S3 with the following structure:
```
s3://{BUCKET_NAME_WITH_BASE_VERSION}/assets/{VERSION}/
```

### Asset Loading in SDK

The SDK fetches assets from S3 using the following logic:

**File**: [`src/hooks/S3ApiHook.res`](../../src/hooks/S3ApiHook.res)

- **Production**: `PROD_ASSETS_END_POINT`
- **Sandbox**: `SANDBOX_ASSETS_END_POINT`
- **Integration**: `INTEG_ASSETS_END_POINT`

### Version Management

- **v1**: Stable version (will deprecate and support for some time)
- **v2**: Current stable version

## Best Practices

1. **Versioning**: Always use version directories (v1, v2) for backward compatibility
2. **Compression**: Gzip large JSON files to reduce bandwidth v1 deprecated 
3. **Immutable Assets**: Never modify existing files; create new versions instead
6. **Documentation**: Update this README when adding new asset types or workflows
