local fn = vim.fn
local keymap = vim.api.nvim_set_keymap

-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

-- Install your plugins here
return packer.startup(function(use)
	-- My plugins here

	use({ "wbthomason/packer.nvim" })

	use({ "EdenEast/nightfox.nvim" }) -- colorscheme
	use({ "nvim-lualine/lualine.nvim",
		requires = { "kyazdani42/nvim-web-devicons" },
		config = function() require("lualine_evil") end }) -- modeline
	use({ "nvim-treesitter/nvim-treesitter" }) -- syntax recognition
	use({ "nvim-telescope/telescope.nvim", -- fuzzyfinder
		requires = {{ "nvim-lua/plenary.nvim" }},
		config = function()
			require("which-key").register {
				["<C-p>"] = { "<cmd>Telescope find_files<cr>", "Find File" },
			}
		end })
	use({ "nvim-telescope/telescope-frecency.nvim",
		requires = {{ "kkharji/sqlite.lua" }},
		config = function()
			require("telescope").load_extension("frecency")
		end })
	use({ "folke/which-key.nvim", -- keybind help
		config = function()
			require("which-key").setup {}
		end })
	use({ "williamboman/mason.nvim",
		config = function()
			require("mason").setup()
		end })
	use({ "williamboman/mason-lspconfig.nvim",
		config = function()
			require("mason-lspconfig").setup()
			require("mason-lspconfig").setup_handlers {
				function(server_name) require("lspconfig")[server_name].setup {
					capabilities = require("cmp_nvim_lsp").default_capabilities(
						vim.lsp.protocol.make_client_capabilities()
					),
					on_attach = function(client, bufnr)
						if client.server_capabilities.documentSymbolProvider then
							require("nvim-navic").attach(client, bufnr)
						end end
				} end } end })
	use({ "neovim/nvim-lspconfig" })
	use({ "hrsh7th/nvim-cmp",
		config = function()
			local cmp = require("cmp")
			cmp.setup {
				sources = cmp.config.sources {
					{ name = "nvim_lsp" }
				},
				mapping = {
				 	["<C-l>"] = cmp.mapping.complete()
				}} end })
	use({ "hrsh7th/cmp-nvim-lsp" })
	use({ "hrsh7th/cmp-buffer" })
	use({ "hrsh7th/cmp-path" })
	use({ "lewis6991/gitsigns.nvim",
		config = function()
			require("gitsigns").setup()
			require("scrollbar.handlers.gitsigns").setup()
		end })
	use({ "TimUntersberger/neogit",
		config = function()
			local neogit = require("neogit")
			neogit.setup {}
		end })
	use({ "nvim-tree/nvim-web-devicons" })
	use({ "SmiteshP/nvim-navic",
		requires = "neovim/nvim-lspconfig",
		config = function()
			require("mason-lspconfig").setup_handlers {
				function(server_name) require("lspconfig")[server_name].setup {
					on_attach = function(client, bufnr)
						if client.server_capabilities.documentSymbolProvider then
							require("nvim-navic").attach(client, bufnr)
						end end	} end } end })
	use({ "petertriho/nvim-scrollbar",
		config = function() require("scrollbar").setup {} end })
	use({ "kevinhwang91/nvim-hlslens",
		config = function() 
			require("hlslens").setup {}
			require("scrollbar.handlers.search").setup {}
		end })
	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end )
