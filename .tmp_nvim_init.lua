require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- LSP Configurations
  use 'neovim/nvim-lspconfig'             -- Collection of configurations for built-in LSP client
  use 'williamboman/mason.nvim'           -- Mason for managing LSP servers
  use 'williamboman/mason-lspconfig.nvim' -- Integration with mason and lspconfig

  -- Autocompletion plugins
  use 'hrsh7th/nvim-cmp'                  -- Autocompletion plugin
  use 'hrsh7th/cmp-nvim-lsp'              -- LSP source for nvim-cmp
  use 'L3MON4D3/LuaSnip'                  -- Snippet engine
  use 'saadparwaiz1/cmp_luasnip'          -- Snippet completions

  -- Treesitter for syntax highlighting
  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate'
  }

  -- Telescope for fuzzy finding
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  -- Lualine for statusline
  use 'nvim-lualine/lualine.nvim'

  -- Git integration
  use 'lewis6991/gitsigns.nvim'

  -- Which-key for keybinding hints
  use 'folke/which-key.nvim'

  -- Additional plugins can be added here
end)

-- Setup Mason
require('mason').setup()

-- Setup Mason-LSPConfig
require('mason-lspconfig').setup {
  ensure_installed = { "clangd", "pyright", "rust_analyzer" }, -- List of servers you want to install
}

-- Update capabilities for nvim-cmp integration
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

-- Setup nvim-lspconfig with Mason-LSPConfig
local lspconfig = require('lspconfig')

vim.filetype.add({
  extension = {
    cblc = 'cblc',
  },
})

require('mason-lspconfig').setup_handlers {
  function(server_name)
    lspconfig[server_name].setup {
      capabilities = capabilities,
      -- You can add custom settings for each server here
    }
  end,
}

vim.lsp.config('cblc_lsp', {
  cmd = { '/home/bvangene/ctoc-transpiler/cblc_lsp' },
  filetypes = { 'cblc' },
  root_markers = { '.git', 'Makefile' },
  single_file_support = true,
  capabilities = capabilities,
})

vim.lsp.enable('cblc_lsp')

local cblc_keywords = {
  'class', 'struct', 'private', 'public', 'import', 'const',
  'void', 'int', 'float', 'double', 'char', 'string', 'bool',
  'if', 'else', 'while', 'for', 'return', 'display', 'true', 'false',
  'std::malloc', 'std::free', 'std::strlen', 'std::strcpy', 'std::strcat',
  'std::strcmp', 'std::atoi', 'std::atol', 'std::atoll', 'std::strtod',
  'std::abs', 'std::fabs', 'std::ceil', 'std::floor', 'std::rounded',
  'std::banker_round', 'std::cos', 'std::sin', 'std::tan', 'std::sqrt',
  'std::log', 'std::exp', 'std::min', 'std::max', 'std::powerof',
  'std::tolower', 'std::toupper', 'std::isdigit', 'std::isalpha',
}

local function cblc_collect_buffer_words(bufnr)
  local seen = {}
  local items = {}
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

  for _, keyword in ipairs(cblc_keywords) do
    seen[keyword] = true
    table.insert(items, keyword)
  end
  for _, line in ipairs(lines) do
    for word in line:gmatch('[A-Za-z_][A-Za-z0-9_:]*') do
      if not seen[word] then
        seen[word] = true
        table.insert(items, word)
      end
    end
  end
  table.sort(items)
  return items
end

_G.cblc_complete = function(findstart, base)
  local line = vim.api.nvim_get_current_line()
  local col = vim.api.nvim_win_get_cursor(0)[2]

  if findstart == 1 then
    local start = col
    while start > 0 do
      local char = line:sub(start, start)
      if not char:match('[%w_:]') then
        break
      end
      start = start - 1
    end
    return start
  end

  local items = cblc_collect_buffer_words(vim.api.nvim_get_current_buf())
  local matches = {}
  local prefix = base:lower()

  for _, item in ipairs(items) do
    if item:lower():find('^' .. vim.pesc(prefix)) then
      table.insert(matches, item)
    end
  end
  return matches
end

vim.api.nvim_create_autocmd('FileType', {
  pattern = 'cblc',
  callback = function(args)
    vim.bo[args.buf].omnifunc = 'v:lua.cblc_complete'
    vim.bo[args.buf].commentstring = '// %s'
  end,
})

-- Diagnostics: show the actual LSP error/warning text inline (end-of-line),
-- instead of only a red "E" sign in the gutter.
vim.diagnostic.config({
  virtual_text = {
    -- Put the message at the end of the line.
    spacing = 2,
    prefix = '',
    source = 'if_many',
  },
  signs = false,          -- disable gutter "E/W" signs
  underline = true,
  update_in_insert = false,
  severity_sort = true,
  float = {
    border = 'rounded',
    source = 'always',
  },
})

-- Setup nvim-cmp for autocompletion
local cmp = require('cmp')
local luasnip = require('luasnip')

local cblc_cmp_source = {}

function cblc_cmp_source.new()
  return setmetatable({}, { __index = cblc_cmp_source })
end

function cblc_cmp_source:is_available()
  return vim.bo.filetype == 'cblc'
end

function cblc_cmp_source:get_trigger_characters()
  return { ':', '_' }
end

function cblc_cmp_source:complete(params, callback)
  local items = cblc_collect_buffer_words(vim.api.nvim_get_current_buf())
  local completions = {}
  local prefix = (params.context.cursor_before_line:match('[A-Za-z_][A-Za-z0-9_:]*$') or ''):lower()

  for _, item in ipairs(items) do
    if prefix == '' or item:lower():find('^' .. vim.pesc(prefix)) then
      table.insert(completions, {
        label = item,
        kind = cmp.lsp.CompletionItemKind.Text,
      })
    end
  end
  callback({ items = completions, isIncomplete = false })
end

cmp.register_source('cblc_local', cblc_cmp_source.new())

-- Function to check if there are words before the cursor
local has_words_before = function()
  local unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  if col == 0 then
    return false
  end
  local line_text = vim.api.nvim_buf_get_lines(0, line -1, line, true)[1]
  local char_before_cursor = line_text:sub(col, col)
  return not char_before_cursor:match("%s")
end

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body) -- For LuaSnip users
    end,
  },
  mapping = {
    -- Define your key mappings for completion
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),

    ['<Down>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
          cmp.select_next_item()
      else
          fallback()
      end
    end, { 'i', 's' }),

    ['<Up>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
          cmp.select_prev_item()
      else
          fallback()
      end
    end, { 'i', 's' }),

    -- Add more mappings according to your preference
  },
  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'cblc_local' },
    { name = 'luasnip' },
  }),
})

cmp.setup.filetype('cblc', {
  sources = cmp.config.sources({
    { name = 'cblc_local' },
    { name = 'nvim_lsp' },
  }),
})

-- Set tabs to 4 spaces instead of 8
vim.o.tabstop = 4        -- The width of a hard tabstop measured in "spaces"
vim.o.shiftwidth = 4     -- The size of an indent
vim.o.expandtab = false  -- Use tabs instead of spaces

-- Enable true color support
vim.o.termguicolors = true  -- Ensure this is set to true
vim.cmd('syntax on')

-- Custom Colorscheme Settings
-- Define custom highlight groups for syntax elements
-- Color codes:
-- light blueish: '#5FD7FF'
-- bright yellow: '#FFFF87'
-- light purple: '#AF87FF'
-- green: '#87D700'
-- bright reddish: '#FF5F5F'

-- Internal functions (your own libraries): light blueish
vim.api.nvim_set_hl(0, '@function', { fg = '#5FD7FF' })

-- External functions (like malloc): light blueish bold
vim.api.nvim_set_hl(0, '@function.builtin', { fg = '#5FD7FF', bold = true })

-- Keywords (if, for, return): bright yellow
vim.api.nvim_set_hl(0, '@keyword', { fg = '#FFFF87' })

-- Variable names: light purple
vim.api.nvim_set_hl(0, '@variable', { fg = '#AF87FF' })

-- String literals: green
vim.api.nvim_set_hl(0, '@string', { fg = '#87D700' })

-- Errors: bright reddish
vim.api.nvim_set_hl(0, '@error', { fg = '#FF5F5F' })

-- Ensure Treesitter is loaded and configured
require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    -- You can disable slow treesitter highlight for large files
    -- disable = function(lang, buf)
    --     local max_filesize = 100 * 1024 -- 100 KB
    --     local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
    --     if ok and stats and stats.size > max_filesize then
    --         return true
    --     end
    -- end,
  },
  -- Add other Treesitter configurations if necessary
}

-- Additional configurations can be added below
