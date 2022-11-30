local opts = { noremap = true, silent = true}
local keymap = vim.api.nvim_set_keymap

-- dvorak compatible
keymap("n", "t", "j", { noremap = true })
keymap("n", "n", "k", { noremap = true })
keymap("n", "s", "l", { noremap = true })
