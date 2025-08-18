# 🌱 Carbonix - Carbon Offset Marketplace

A decentralized marketplace for carbon offset credits built on the Stacks blockchain. Carbonix enables the creation, validation, trading, and retirement of tokenized carbon offset credits with oracle-based verification.

## 🚀 Features

- **🏭 Offset Creation**: Create carbon offset projects with customizable parameters
- **✅ Oracle Validation**: Third-party validation of carbon offset projects
- **💰 Marketplace Trading**: Buy and sell validated carbon credits
- **🔥 Credit Retirement**: Permanently retire credits to offset carbon footprint
- **📊 Transparent Tracking**: Full transparency of all transactions and balances
- **💸 Platform Fees**: Configurable platform fees for sustainability

## 📋 Contract Functions

### Public Functions

#### `create-offset`
Create a new carbon offset project
```clarity
(create-offset amount price-per-credit description expires-at)
```

#### `validate-offset`
Validate an offset project (oracle only)
```clarity
(validate-offset offset-id)
```

#### `list-for-sale`
List carbon credits for sale in the marketplace
```clarity
(list-for-sale offset-id amount price-per-credit)
```

#### `purchase-credits`
Purchase carbon credits from a marketplace listing
```clarity
(purchase-credits listing-id amount)
```

#### `retire-credits`
Permanently retire carbon credits
```clarity
(retire-credits amount)
```

#### `cancel-listing`
Cancel a marketplace listing
```clarity
(cancel-listing listing-id)
```

### Read-Only Functions

- `get-offset-details` - Get details of a carbon offset project
- `get-listing-details` - Get marketplace listing information
- `get-user-credit-balance` - Check user's carbon credit balance
- `get-user-purchases` - Get user's purchase history
- `get-platform-fee-rate` - Current platform fee rate

## 🛠️ Usage Instructions

### 1. Deploy the Contract
```bash
clarinet deploy
```

### 2. Set Oracle Address (Contract Owner Only)
```clarity
(contract-call? .carbonix set-oracle 'SP1ORACLE...)
```

### 3. Create Carbon Offset Project
```clarity
(contract-call? .carbonix create-offset u1000 u50 "Solar farm project" u1000000)
```

### 4. Validate Project (Oracle Only)
```clarity
(contract-call? .carbonix validate-offset u1)
```

### 5. List Credits for Sale
```clarity
(contract-call? .carbonix list-for-sale u1 u100 u55)
```

### 6. Purchase Credits
```clarity
(contract-call? .carbonix purchase-credits u1 u50)
```

### 7. Retire Credits
```clarity
(contract-call? .carbonix retire-credits u25)
```

## 🔧 Configuration

- **Platform Fee**: Default 2.5% (250 basis points), configurable by contract owner
- **Oracle Validation**: Required before credits can be minted and traded
- **Expiration**: Offset projects have configurable expiration dates

## 🏗️ Development

### Prerequisites
- Clarinet CLI
- Stacks blockchain testnet/mainnet access

### Testing
```bash
clarinet test
```

### Local Development
```bash
clarinet console
```

## 📊 Token Economics

- **Carbon Credits**: Fungible tokens representing verified carbon offsets
- **1:1 Ratio**: Each token represents one unit of carbon offset
- **Retirement**: Credits are burned when retired, removing them from circulation
- **Platform Fees**: Small percentage fee on marketplace transactions

## 🔒 Security Features

- Oracle-based validation prevents fraudulent offsets
- Time-based expiration for offset projects
- Owner-only administrative functions
- Balance checks prevent overselling
- Immutable retirement records

## 🌍 Environmental Impact

Carbonix promotes environmental sustainability by:
- Facilitating transparent carbon offset trading
- Ensuring verified environmental impact through oracles
- Enabling easy carbon footprint offsetting
- Creating economic incentives for carbon reduction projects

