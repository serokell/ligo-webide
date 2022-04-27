'use strict'

const coin = `pragma solidity >=0.5.0 <0.7.0;

contract Coin {
  address public minter;
  mapping (address => uint) private balances;

  event Sent(address from, address to, uint amount);

  constructor() public {
    minter = msg.sender;
  }

  function mint(address receiver, uint amount) public {
    require(msg.sender == minter);
    require(amount < 1e60);
    balances[receiver] += amount;
  }

  function send(address receiver, uint amount) public {
    require(amount <= balances[msg.sender], "Insufficient balance.");
    balances[msg.sender] -= amount;
    balances[receiver] += amount;
    emit Sent(msg.sender, receiver, amount);
  }
  
  function balanceOf(address tokenOwner) public view returns(uint balance){
    return balances[tokenOwner];
  }
}
`

const mDeploy = `const Coin = artifacts.require('Coin')

module.exports = async function (deployer, network, accounts) {
  await deployer.deploy(Coin)
}
`

const deploy = `async function main() {
  const Coin = await ethers.getContractFactory('Coin')
  const deployed = await Coin.deploy()
  console.log('Contract deployed to:', deployed.address)
}
  
main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error)
    process.exit(1)
  })

`

const waffleDeploy = `const { ethers } = require('ethers')
const Coin = require('../build/contracts/Coin.json')

const provider = ethers.getDefaultProvider('http://localhost:62743')
const signer = provider.getSigner()

async function main() {
  const factory = new ethers.ContractFactory(Coin.abi, Coin.bytecode, signer)
  const deployed = await factory.deploy()
  console.log('Contract deployed to:', deployed.address)
}

main()
`

const readme = `# Coin`

const config = `{
  "main": "./contracts/Coin.sol",
  "deploy": "./build/contracts/Coin.json",
  "framework": "#framework",
  "compilers": {
    "solc": "0.6.12"
  }
}
`

const truffleConfig = `module.exports = {}`

export const getExamples = (name) => {
  return {
    storage: { name: `.workspaces/${name}/contracts/Coin.sol`, content: coin },

    ballot_test: { name: `.workspaces/${name}/migrations/1.deploy.js`, content: mDeploy },

    deploy: { name: `.workspaces/${name}/scripts/deploy.js`, content: deploy },
    waffleDeploy: { name: `.workspaces/${name}/scripts/waffle-deploy.js`, content: waffleDeploy },

    readme: { name: `.workspaces/${name}/README.md`, content: readme },
    config: { name: `.workspaces/${name}/config.json`, content: config },
    truffleConfig: { name: `.workspaces/${name}/truffle-config.js`, content: truffleConfig }
  }
}
