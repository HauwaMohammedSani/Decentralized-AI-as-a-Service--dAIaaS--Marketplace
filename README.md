A blockchain-powered platform for publishing, licensing, and monetizing AI/ML models using Clarity smart contracts on Stacks.

## 🚀 Features

- **🏪 Model Marketplace**: Register and discover AI models
- **🎫 NFT Licensing**: Purchase usage rights as transferable NFTs  
- **💰 Pay-per-Inference**: Granular metering and billing
- **📊 Usage Tracking**: On-chain inference monitoring
- **🔍 Audit System**: Community-driven model validation
- **🗳️ DAO Governance**: Decentralized quality control
- **⭐ User Feedback System**: Rate and review AI models

## 📋 Core Functions

### Model Management
- `register-model` - 📝 Register a new AI model
- `deactivate-model` - ⏸️ Disable model listing
- `get-model` - 📖 Retrieve model information

### Licensing & Usage  
- `purchase-license` - 🎫 Buy inference credits as NFT
- `use-inference` - ⚡ Execute one model inference
- `transfer` - 🔄 Transfer license NFT

### Financial Operations
- `deposit-funds` - 💳 Add STX to marketplace balance
- `withdraw-funds` - 💸 Withdraw earnings
- `get-user-balance` - 💰 Check account balance

### Quality Control
- `audit-model` - 🔍 Submit model audit
- `vote-on-model` - 🗳️ DAO governance voting
- `submit-feedback` - ⭐ Rate and review models
- `get-feedback` - 📝 View user feedback
- `get-average-rating` - 📊 Get model rating stats

## 🏗️ Usage Instructions

### 1. Deploy Contract
```bash
clarinet deploy --testnet
```

### 2. Register AI Model
```clarity
(contract-call? .daiaaS-marketplace register-model 
  "GPT-Style Model" 
  "Advanced language processing model"
  u1000
  "ipfs://model-metadata-hash"
)
```

### 3. Add Funds to Marketplace
```clarity
(contract-call? .daiaaS-marketplace deposit-funds u50000)
```

### 4. Purchase Model License
```clarity
(contract-call? .daiaaS-marketplace purchase-license 
  u1        ;; model-id
  u100      ;; number of inferences
  u100000   ;; payment amount
)
```

### 5. Use AI Inference
```clarity
(contract-call? .daiaaS-marketplace use-inference u1)
```

### 6. Audit Model Quality
```clarity
(contract-call? .daiaaS-marketplace audit-model
  u1
  u85
  "High quality model with good accuracy"
)
```

### 7. Submit Model Feedback
```clarity
(contract-call? .daiaaS-marketplace submit-feedback
  u1        ;; model-id
  u5        ;; rating (1-5)
  "Excellent performance and accuracy"
)
```

## 💡 Contract Architecture

### Data Structures
- **AI Models**: Metadata, pricing, usage stats
- **Licenses**: NFT-based usage rights with expiration
- **Audits**: Community quality assessments
- **Balances**: STX holdings for users
- **Votes**: DAO governance participation
- **Feedback**: User ratings and reviews for models

### Key Variables
- `platform-fee`: 2.5% platform commission
- `last-model-id`: Auto-incrementing model counter
- `last-license-id`: Auto-incrementing license counter

## 🔒 Security Features

- ✅ Owner-only model deactivation
- ✅ License expiration (1000 blocks)
- ✅ Balance validation before transactions
- ✅ NFT ownership verification
- ✅ Platform fee collection

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

## 📄 License

MIT License - Build the future of decentralized AI! 🌟

---

*Empowering developers to monetize AI models without platform lock-in* 🔓
