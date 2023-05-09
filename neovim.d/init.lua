local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		'git',
		'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

vim.wo.number = true
vim.wo.relativenumber = true

require('lazy').setup({
{ 'EdenEast/nightfox.nvim',
	lazy = false,
	priority = 100,
	config = function() require('nightfox').load() end },
{ 'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
	event = 'VeryLazy',
	opts = { require('lualine_evil') }},
{ 'akinsho/bufferline.nvim',
	event = 'VeryLazy',
	dependencies = 'nvim-tree/nvim-web-devicons',
	config = true },
{ 'nvim-treesitter/nvim-treesitter',
	main = 'nvim-treesitter.configs',
	opts = {
		ensure_installed = {},
		auto_install = true,
		highlight = { enable = true, additional_vim_regex_highlighting = false },
		indent = { enable = true }}},
{ 'nvim-treesitter/nvim-treesitter-textobjects',
	dependencies = { 'nvim-treesitter/nvim-treesitter' }},
{ 'nvim-telescope/telescope.nvim',
        dependencies = { 'nvim-lua/plenary.nvim' },
	cmd = 'Telescope',
        config = function()
		local tele = require('telescope.builtin')
                require('which-key').register {
			['<C-x>'] = { tele.commands, 'Commands' },
                        ['<C-f>'] = { tele.find_files, 'Find Files' },
			['<C-s>'] = { tele.live_grep, 'Live Grep' },
			['<C-b>'] = { tele.buffers, 'Buffers' },
			['<C-h>'] = { tele.help_tags, 'Help' },
                }
        end },
{ 'nvim-telescope/telescope-frecency.nvim',
        dependencies = { 'kkharji/sqlite.lua' },
        config = function()
                require('telescope').load_extension('frecency')
        end },
{ 'folke/which-key.nvim' },
{ 'williamboman/mason.nvim',
	build = ':MasonUpdate',
	config = true },
{ 'williamboman/mason-lspconfig.nvim',
	dependencies = { 'williamboman/mason.nvim' },
	config = true },
{ 'neovim/nvim-lspconfig',
	dependencies = { 'williamboman/mason-lspconfig.nvim' },
	config = function()
		local lsp = vim.lsp.buf
		require('which-key').register {
			['gd'] = { lsp.definition, 'jump to definition' },
			['gD'] = { lsp.declaration, 'jump to declaration' },
			['K'] = { lsp.hover, 'show hover info' },
			['gi'] = { lsp.implementation, 'jump to implementation' }} end },
{ 'nvim-neo-tree/neo-tree.nvim',
	dependencies = { 'nvim-lua/plenary.nvim', 'nvim-tree/nvim-web-devicons', 'MunifTanjim/nui.nvim' }},
{ 'mfussenegger/nvim-dap' },
{ 'rcarriga/nvim-dap-ui',
	dependencies = { 'mfussenegger/nvim-dap' }},
{ 'jay-babu/mason-nvim-dap.nvim',
	dependencies = { 'williamboman/mason.nvim', 'mfussenegger/nvim-dap' }},
{ 'echasnovski/mini.pairs' },
{ 'L3MON4D3/LuaSnip' },
{ 'hrsh7th/nvim-cmp',
        dependencies = { 'dcampos/cmp-snippy', 'dcampos/nvim-snippy' },
        opts = function() 
		local cmp = require('cmp')
		return {
        	snippet = {
                        expand = function(args)
                                require('snippy').expand_snippet(args.body)
                        end },
                sources = cmp.config.sources {
                        { name = 'nvim_lsp' },
                        { name = 'buffer' },
                },
                mapping = cmp.mapping.preset.insert({
                        ['<C-Space>'] = cmp.mapping.complete(),
			['<C-g>'] = cmp.mapping.abort(),
                        ['<CR>'] = cmp.mapping.confirm({ select = true })})} end },
{ 'hrsh7th/cmp-nvim-lsp' },
{ 'hrsh7th/cmp-buffer' },
{ 'hrsh7th/cmp-path' },
{ 'lewis6991/gitsigns.nvim',
	dependencies = { 'nvim-scrollbar' },
	event = { 'BufNewFile', 'BufRead' }},
{ 'lukas-reineke/indent-blankline.nvim',
	event = { 'BufNewFile', 'BufRead' }},
{ 'TimUntersberger/neogit' },
{ 'SmiteshP/nvim-navic',
        requires = 'neovim/nvim-lspconfig',
        config = function()
                require('mason-lspconfig').setup_handlers {
                        function(server_name) require('lspconfig')[server_name].setup {
                                on_attach = function(client, bufnr)
                                        if client.server_capabilities.documentSymbolProvider then
                                                require('nvim-navic').attach(client, bufnr)
                                        end end } end } end },
{ 'petertriho/nvim-scrollbar',
        cond = function() return not vim.g.vscode end,
	name = 'scrollbar' },
{ 'kevinhwang91/nvim-hlslens',
	enabled = false,
	dependencies = { 'petertriho/nvim-scrollbar' },
	config = true },
{ 'nvim-neorg/neorg',
        run = ':Neorg sync-parsers',
	ft = 'neorg',
        opts = {
                ['core.defaults'] = {},
                ['core.dirman'] = {
                        config = {
                                workspaces = {
                                        work = '~/Note/work',
                                        home = '~/Note/home' }}}}},
{ 'rust-lang/rust.vim',
	ft = 'rs' },
},{
defaults = {}})

--require('mason').setup()
--[[
require('lazy').setup({
	'folke/which-key.nvim',
	{'EdenEast/nightfox.nvim',
	lazy = false,
	priority = 1000,
	config = function()
		require('nightfox').load()
	end},
	{'AlexvZyl/nordic.nvim',
	enabled = false,
	lazy = false,
	config = function()
		require('nordic').load()
	end},
	'nvim-lualine/lualine.nvim',
	'nvim-treesitter/nvim-treesitter',
	'nvim-telescope/telescope.nvim',
	'nvim-telescope/telescope-frecency.nvim',
	'williamboman/mason.nvim',
	'williamboman/mason-lspconfig.nvim',
	'neovim/nvim-lspconfig',
	'hrsh7th/nvim-cmp',
	'hrsh7th/cmp-nvim-lsp',
	'hrsh7th/cmp-buffer',
	'hrsh7th/cmp-path',
	'lewis6991/gitsigns.nvim',
	'TimUntersberger/neogit',
	'nvim-tree/nvim-web-devicons',
	'SmiteshP/nvim-navic',
	'petertriho/nvim-scrollbar',
	{'kevinhwang91/nvim-hlslens', main = 'hlslens', config = true, lazy = false},
	'mfussenegger/nvim-dap',
	'rcarriga/nvim-dap-ui',
	'nvim-neorg/neorg',
	'rust-lang/rust.vim'
})
]]
