local util = require 'lspconfig.util'

return {
    default_config = {
        cmd = { 'daedalus-language-server' },
        filetypes = { 'daedalus' },
        root_dir = function(filepath)
            return util.path.dirname(filepath)
        end,
    },
    docs = {
        description = [[
https://github.com/GaloisInc/daedalus/tree/master/daedalus-language-server

Daedalus Language Server]],
    },
}
