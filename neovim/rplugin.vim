" node plugins
call remote#host#RegisterPlugin('node', '/Users/jfelice/src/dotfiles/neovim/config/nvim.symlink/bundle/nvim-parinfer.js/rplugin/node/nvim-parinfer.js', [
      \ {'sync': v:true, 'name': 'ParinferIndent', 'type': 'function', 'opts': {'eval': '[getpos(''.''), bufnr(''.''), getline(1,line(''$'')), g:parinfer_mode, g:parinfer_preview_cursor_scope, v:operator, -strlen(@-)]'}},
      \ {'sync': v:true, 'name': 'ParinferShift', 'type': 'function', 'opts': {'eval': '[getline(1,line(''$''))]'}},
     \ ])
call remote#host#RegisterPlugin('node', '/Users/jfelice/src/dotfiles/neovim/config/nvim.symlink/bundle/clj-refactor.nvim/rplugin/node/clj-refactor.js', [
      \ {'sync': v:false, 'name': 'CAddDeclaration', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CCycleColl', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CCycleIf', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CCyclePrivacy', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CCycleThread', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CExpandLet', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CExtractDef', 'type': 'command', 'opts': {'nargs': 1, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CFunctionFromExample', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CIntroduceLet', 'type': 'command', 'opts': {'nargs': 1, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CMoveToLet', 'type': 'command', 'opts': {'nargs': 1, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CThread', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CThreadFirstAll', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CThreadLast', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CThreadLastAll', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CUnwindAll', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CUnwindThread', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CFormatAll', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CFormatForm', 'type': 'command', 'opts': {'nargs': 0, 'eval': 'getpos(''.'')'}},
      \ {'sync': v:false, 'name': 'CAddMissingLibSpec', 'type': 'command', 'opts': {'nargs': 0, 'eval': '[getpos(''.''), expand(''<cword>'')]'}},
      \ {'sync': v:false, 'name': 'CCleanNS', 'type': 'command', 'opts': {'nargs': 0, 'eval': '[getpos(''.''), expand(''%:p''), (exists(''g:clj_refactor_prune_ns_form'') ? g:clj_refactor_prune_ns_form : 1), (exists(''g:clj_refactor_prefix_rewriting'') ? g:clj_refactor_prefix_rewriting : 1)]'}},
      \ {'sync': v:false, 'name': 'CRenameFile', 'type': 'command', 'opts': {'complete': 'file', 'nargs': 1, 'eval': 'expand(''%:p'')'}},
      \ {'sync': v:false, 'name': 'CRenameDir', 'type': 'command', 'opts': {'complete': 'dir', 'nargs': 1, 'eval': 'expand(''%:p:h'')'}},
      \ {'sync': v:false, 'name': 'CRenameSymbol', 'type': 'command', 'opts': {'nargs': 1, 'eval': '[getcwd(), expand(''%:p''), fireplace#ns(), expand(''<cword>''), fireplace#info(expand(''<cword>'')), getpos(''.'')]'}},
      \ {'sync': v:false, 'name': 'CExtractFunction', 'type': 'command', 'opts': {'nargs': 1, 'eval': '[expand(''%:p''), getpos(''.'')]'}},
      \ {'sync': v:true, 'name': 'CMagicRequires', 'type': 'command', 'opts': {'nargs': 0, 'eval': '[getpos(''.''), expand(''%:p''), expand(''<cword>'')]'}},
     \ ])


" python3 plugins


" ruby plugins


" python plugins


