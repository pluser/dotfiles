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

local function fullspec()
	return not vim.g.vscode
end

require('lazy').setup({
{ 'EdenEast/nightfox.nvim',
	lazy = false,
	priority = 100,
	config = function(plugin, opts) require('nightfox').load() end },
{ 'nvim-lualine/lualine.nvim',
        dependencies = { 'nvim-tree/nvim-web-devicons' },
	event = { 'VeryLazy' },
	config = function(plugin, opts) require('lualine_evil') end },
{ 'akinsho/bufferline.nvim',
	event = 'VeryLazy',
	dependencies = { 'nvim-tree/nvim-web-devicons' },
	config = true },
{ 'nvim-treesitter/nvim-treesitter',
	build = ':TSUpdate',
	config = function(name, opts) require('nvim-treesitter').setup(opts) end,
	event = { 'BufNewFile', 'BufRead' },
	opts = {
		ensure_installed = {},
		auto_install = true,
		highlight = { enable = true, additional_vim_regex_highlighting = false },
		indent = { enable = true }}},
{ 'nvim-treesitter/nvim-treesitter-textobjects',
	lazy = true,
	event = { 'BufNewFile', 'BufRead' },
	dependencies = { 'nvim-treesitter/nvim-treesitter' }},
{ 'nvim-telescope/telescope.nvim',
        dependencies = { 'nvim-lua/plenary.nvim', 'nvim-telescope/telescope-frecency.nvim' },
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
		require('telescope').load_extension('frecency')
        end },
{ 'nvim-telescope/telescope-frecency.nvim',
	lazy = true,
        dependencies = { 'kkharji/sqlite.lua' }},
{ 'folke/which-key.nvim' },
{ 'williamboman/mason.nvim',
	event = { 'BufRead' },
	cmd = { 'Mason', 'MasonInstall' },
	build = ':MasonUpdate',
	config = true },
{ 'williamboman/mason-lspconfig.nvim',
	event = { 'LspAttach' },
	dependencies = { 'williamboman/mason.nvim' }},
{ 'neovim/nvim-lspconfig',
	event = { 'LspAttach' },
	dependencies = { 'williamboman/mason-lspconfig.nvim' },
	config = function()
		local lsp = vim.lsp.buf
		require('which-key').register {
			['gd'] = { lsp.definition, 'jump to definition' },
			['gD'] = { lsp.declaration, 'jump to declaration' },
			['K'] = { lsp.hover, 'show hover info' },
			['gi'] = { lsp.implementation, 'jump to implementation' }} end },
{ 'nvim-neo-tree/neo-tree.nvim',
	cmd = 'Neotree',
	dependencies = { 'nvim-lua/plenary.nvim', 'nvim-tree/nvim-web-devicons', 'MunifTanjim/nui.nvim' }},
{ 'mfussenegger/nvim-dap',
	lazy = true },
{ 'rcarriga/nvim-dap-ui',
	event = { 'LspAttach' },
	dependencies = { 'mfussenegger/nvim-dap' }},
{ 'jay-babu/mason-nvim-dap.nvim',
	event = { 'LspAttach' },
	dependencies = { 'williamboman/mason.nvim', 'mfussenegger/nvim-dap' },
	opts = {
		handlers = {
			python = function(source_name)
				local dap = require('dap')
				dap.adapters.python = {
					type = 'executable',
					command = 'python3',
					args = {
						'-m',
						'debugpy.adapter',
					},
				}
				dap.configurations.python = {
					{
						type = 'python',
						request = 'launch',
						name = 'Launch file',
						program = '${file}',
					},
				}
			end,
		},
	} },
{ 'echasnovski/mini.pairs',
	name = 'mini.pairs',
	event = { 'InsertEnter' },
	config = function() require('mini.pairs').setup() end },
{ 'hrsh7th/nvim-cmp',
	event = { 'InsertEnter', 'CmdlineEnter' },
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
{ 'hrsh7th/cmp-nvim-lsp',
	event = { 'InsertEnter' }},
{ 'hrsh7th/cmp-buffer',
	event = { 'InsertEnter' }},
{ 'hrsh7th/cmp-path',
	event = { 'InsertEnter' }},
{ 'dcampos/cmp-snippy',
	event = { 'InsertEnter' }},
{ 'dcampos/nvim-snippy',
	event = { 'InsertEnter' }},
{ 'lewis6991/gitsigns.nvim',
	dependencies = { 'petertriho/nvim-scrollbar' },
	event = { 'BufNewFile', 'BufRead' }},
{ 'lukas-reineke/indent-blankline.nvim',
	event = { 'BufNewFile', 'BufRead' }},
{ 'TimUntersberger/neogit',
	cmd = 'Neogit' },
{ 'SmiteshP/nvim-navic',
	dependencies = { 'neovim/nvim-lspconfig' },
	event = { 'LspAttach' },
        config = function()
                require('mason-lspconfig').setup_handlers {
                        function(server_name) require('lspconfig')[server_name].setup {
                                on_attach = function(client, bufnr)
                                        if client.server_capabilities.documentSymbolProvider then
                                                require('nvim-navic').attach(client, bufnr)
                                        end end } end } end },
{ 'petertriho/nvim-scrollbar',
	event = { 'BufWinEnter', 'CmdwinLeave', 'TabEnter', 'TermEnter', 'TextChanged', 'VimResized', 'WinEnter', 'WinScrolled' },
	name = 'scrollbar' },
{ 'kevinhwang91/nvim-hlslens',
	enabled = false,
	name = 'hlslens' },
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
defaults = {
	cond = fullspec,
}})

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
