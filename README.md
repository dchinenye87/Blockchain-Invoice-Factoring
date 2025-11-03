# 🏦 Blockchain Invoice Factoring

> 💰 Tokenized invoice selling system for liquidity access in small businesses

## 📋 Overview

This smart contract enables small businesses to tokenize their invoices and sell them to investors for immediate liquidity. Built on the Stacks blockchain using Clarity, it provides a decentralized marketplace for invoice factoring.

## ✨ Features

- 📄 **Invoice Creation**: Businesses can create digital invoices with debtor information
- 💸 **Invoice Trading**: List invoices for sale at custom prices
- 🤝 **Offer System**: Buyers can make offers on invoices
- 💳 **Secure Payments**: Built-in escrow system with STX deposits/withdrawals
- 🏛️ **Platform Fees**: Configurable fee structure (default 2.5%)
- 🔒 **Access Control**: Role-based permissions for all operations

## 🚀 Getting Started

### Prerequisites

```bash
clarinet --version
```

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Deploy the contract

```bash
clarinet deploy
```

## 📖 Usage Guide

### 💰 Managing Funds

**Deposit STX to your account:**
```clarity
(contract-call? .blockchain-invoice-factoring deposit u1000000)
```

**Withdraw STX from your account:**
```clarity
(contract-call? .blockchain-invoice-factoring withdraw u500000)
```

**Check your balance:**
```clarity
(contract-call? .blockchain-invoice-factoring get-user-balance tx-sender)
```

### 📄 Invoice Management

**Create a new invoice:**
```clarity
(contract-call? .blockchain-invoice-factoring create-invoice 
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7  ;; debtor
  u1000000                                        ;; amount (10 STX)
  u1000                                          ;; due date (block height)
  "Payment for services rendered")               ;; description
```

**List invoice for sale:**
```clarity
(contract-call? .blockchain-invoice-factoring list-invoice-for-sale 
  u1        ;; invoice-id
  u800000)  ;; sale price (8 STX for 10 STX invoice = 20% discount)
```

**Remove invoice from sale:**
```clarity
(contract-call? .blockchain-invoice-factoring remove-invoice-from-sale u1)
```

### 🛒 Buying & Trading

**Buy an invoice directly:**
```clarity
(contract-call? .blockchain-invoice-factoring buy-invoice u1)
```

**Make an offer on an invoice:**
```clarity
(contract-call? .blockchain-invoice-factoring make-offer 
  u1        ;; invoice-id
  u750000   ;; offer amount
  u500)     ;; expires at block height
```

**Accept an offer (as invoice owner):**
```clarity
(contract-call? .blockchain-invoice-factoring accept-offer 
  u1                                           ;; invoice-id
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7) ;; buyer principal
```

### 💳 Payment Processing

**Pay an invoice (as debtor):**
```clarity
(contract-call? .blockchain-invoice-factoring pay-invoice u1)
```

### 🔍 Query Functions

**Get invoice details:**
```clarity
(contract-call? .blockchain-invoice-factoring get-invoice u1)
```

**Get offer details:**
```clarity
(contract-call? .blockchain-invoice-factoring get-offer 
  u1                                           ;; invoice-id
  'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7) ;; buyer
```

## 🏗️ Contract Architecture

### Data Structures

- **Invoices**: Core invoice data with ownership tracking
- **Offers**: Buyer offers with expiration times  
- **Balances**: User STX balance management

### Key Functions

| Function | Description | Access |
|----------|-------------|---------|
| `create-invoice` | Create new invoice | Anyone |
| `list-invoice-for-sale` | Put invoice on market | Owner only |
| `buy-invoice` | Purchase listed invoice | Anyone (except owner) |
| `make-offer` | Submit purchase offer | Anyone (except owner) |
