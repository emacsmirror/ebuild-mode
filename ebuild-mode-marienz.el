

(require 'sh-script)

(defun ebuild-run-ebuild (command)
  "Run ebuild \"ebuild command buffername\""
  (interactive "sCommand:")
  (compile (format "NOCOLOR=true ebuild %s %s" buffer-file-name command)))

(setq ebuild-core-keywords
      ; defaults
      '("use" "has_version" "best_version" "use_with" "use_enable" "check_KV"
        "keepdir" "econf" "die" "einstall" "einfo" "ewarn" "eerror" "diropts"
        "dobin" "docinto" "dodoc" "doexe" "dohard" "dohtml" "doinfo" "doins"
        "dolib" "dolib.a" "dolib.so" "doman" "dosbin" "dosym" "emake"
        "exeinto" "exeopts" "fowners" "fperms" "insinto" "insopts" "into"
        "libopts" "newbin" "newexe" "newins" "newman" "newsbin" "prepall"
        "prepalldocs" "prepallinfo" "prepallman" "prepallstrip" "has" "unpack"
        "dopython" "dosed" "into" "doinitd" "doconfd" "doenvd" "dojar" "domo"
        "dodir" "ebegin" "eend" "newconfd" "newdoc" "newenvd" "newinitd"
        "newlib.a" "newlib.so" "hasq" "hasv" "useq" "usev"
        ; sandbox
        "addread" "addwrite" "adddeny" "addpredict")
      ebuild-functions-keywords
      '("pkg_nofetch" "pkg_setup" "src_unpack" "src_compile" "src_test"
        "src_install" "pkg_preinst" "pkg_postinst" "pkg_prerm" "pkg_postrm"
        "pkg_config")
      ebuild-inherit-keywords '("inherit")
      ebuild-eclass-keywords ; alist mapping eclass names to their functions
      '(("eutils"
         "gen_usr_ldscript" "draw_line" "epatch" "have_NPTL"
         "get_number_of_jobs" "egetent" "emktemp" "enewuser" "enewgroup"
         "edos2unix" "make_desktop_entry" "unpack_pdv" "unpack_makeself"
         "check_license" "cdrom_get_cds" "cdrom_load_next"
         "cdrom_locate_file_on_cd" "strip-linguas" "epause" "ebeep"
         "built_with_use" "make_session_desktop" "domenu" "doicon"
         "find_unpackable_file" "unpack_pdv" "set_arch_to_kernel"
         "set_arch_to_portage" "preserve_old_lib" "preserve_old_lib_notify"
         "built_with_use" "epunt_cxx" "dopamd" "newpamd" "make_wrapper")
        ("flag-o-matic"
         "setup-allowed-flags" "filter-flags" "filter-lfs-flags"
         "append-lfs-flags" "append-flags" "replace-flags" "replace-cpu-flags"
         "is-flag" "filter-mfpmath" "strip-flags" "test_flag"
         "test_version_info" "strip-unsupported-flags" "get-flag"
         "has_hardened" "has_pic" "has_pie" "has_ssp_all" "has_ssp" "has_m64"
         "has_m32" "replace-sparc64-flags" "append-ldflags" "filter-ldflags"
         "fstack-flags" "gcc2-flags")
        ("gcc"
         "gcc-getCC" "gcc-getCXX" "gcc-fullversion" "gcc-version"
         "gcc-major-version" "gcc-minor-version" "gcc-micro-version"
         "gcc-libpath" "gcc-libstdcxx-version" "gcc-libstdcxx-major-version"
         "gcc2-flags")
        ("libtool"
         "elibtoolize" "uclibctoolize" "darwintoolize")
        ("fixheadtails"
         "ht_fix_file" "ht_fix_all")
        ("fdo-mime"
         "fdo-mime_desktop_database_update" "fdo-mime_mime_database_update")
        ("webapp"
         "webapp_checkfileexists" "webapp_import_config" "webapp_strip_appdir"
         "webapp_strip_d" "webapp_strip_cwd" "webapp_configfile"
         "webapp_hook_script" "webapp_postinst_txt" "webapp_postupgrade_txt"
         "webapp_runbycgibin" "webapp_serverowned" "webapp_server_configfile"
         "webapp_sqlscript" "webapp_src_install" "webapp_pkg_postinst"
         "webapp_pkg_setup" "webapp_getinstalltype" "webapp_src_preinst"
         "webapp_pkg_prerm")
        ("versionator"
         "get_all_version_components" "version_is_at_least"
         "get_version_components" "get_major_version"
         "get_version_component_range" "get_after_major_version"
         "replace_version_separator" "replace_all_version_separators"
         "delete_version_separator" "delete_all_version_separators")
        ("cvs"
         "cvs_fetch" "cvs_src_unpack")
        ("bash-completion"
         "dobashcompletion" "bash-completion_pkg_postinst")
        ("vim-plugin"
         "vim-plugin_src_install" "vim-plugin_pkg_postinst"
         "vim-plugin_pkg_postrm" "update_vim_afterscripts"
         "display_vim_plugin_help")
        ("vim-doc"
         "update_vim_helptags")
        ("multilib"
         "has_multilib_profile" "get_libdir" "get_multilibdir"
         "get_libdir_override" "get_abi_var" "get_abi_CFLAGS"
         "get_abi_LDFLAGS" "get_abi_CHOST" "get_abi_FAKE_TARGETS"
         "get_abi_CDEFINE" "get_abi_LIBDIR" "get_install_abis" "get_all_abis"
         "get_all_libdirs" "is_final_abi" "number_abis" "get_ml_incdir"
         "prep_ml_includes" "create_ml_includes" "create_ml_includes-absolute"
         "create_ml_includes-tidy_path" "create_ml_includes-listdirs"
         "create_ml_includes-makedestdirs" "create_ml_includes-allfiles"
         "create_ml_includes-sym_for_dir")
        ("64-bit"
         "64-bit")
        ("toolchain-funcs"
         "tc-getPROG" "tc-getAR" "tc-getAS" "tc-getCC" "tc-getCXX" "tc-getLD"
         "tc-getNM" "tc-getRANLIB" "tc-getF77" "tc-getGCJ" "tc-getBUILD_CC"
         "tc-export" "ninj" "tc-is-cross-compiler" "tc-ninja_magic_to_arch"
         "tc-arch-kernel" "tc-arch" "tc-endian" "gcc-fullversion"
         "gcc-version" "gcc-major-version" "gcc-minor-version"
         "gcc-micro-version")
        ("cron"
         "docrondir" "docron" "docrontab" "cron_pkg_postinst")
        ("games"
         "egamesconf" "egamesinstall" "gameswrapper" "dogamesbin"
         "dogamessbin" "dogameslib" "dogameslib.a" "dogameslib.so"
         "newgamesbin" "newgamessbin" "gamesowners" "gamesperms"
         "prepgamesdirs" "gamesenv" "games_pkg_setup" "games_src_compile"
         "games_pkg_postinst" "games_ut_unpack" "games_umod_unpack"
         "games_make_wrapper")
        ("subversion"
         "subversion_svn_fetch" "subversion_bootstrap"
         "subversion_src_unpack")
        ("alternatives"
         "alternatives_auto_makesym" "alternatives_makesym"
         "alternatives_pkg_postinst" "alternatives_pkg_postrm")
        ("rpm"
         "rpm_unpack rpm_src_unpack")
        ("python"
         "python_version" "python_tkinter_exists" "python_mod_exists"
         "python_mod_compile" "python_mod_optimize" "python_mod_cleanup"
         "python_makesym" "python_disable_pyc" "python_enable_pyc")
        ("check-kernel"
         "check_version_h" "get_KV_info" "is_2_4_kernel" "is_2_5_kernel"
         "is_2_6_kernel" "kernel_supports_modules")
        ("perl-module"
         "perl-module_src_prep" "perl-module_src_compile"
         "perl-module_src_test" "perl-module_src_install"
         "perl-module_pkg_setup" "perl-module_pkg_preinst"
         "perl-module_pkg_postinst" "perl-module_pkg_prerm"
         "perl-module_pkg_postrm" "perlinfo" "fixlocalpod" "updatepod")
        ("distutils"
         "distutils_src_compile" "distutils_src_install"
         "distutils_pkg_postrm" "distutils_pkg_postinst"
         "distutils_python_version" "disutils_python_tkinter")
        ("depend.apache"
         "need_apache" "need_apache1" "need_apache2")
        ("apache-module"
         "apache-module_pkg_setup" "apache-module_src_compile"
         "apache-module_src_install" "apache-module_pkg_postinst"
         "acache_cd_dir" "apache_mod_file" "apache_doc_magic"
         "apache1_src_compile" "apache1_src_install"
         "apache1_pkg_postinst" "apache2_pkg_setup"
         "apache2_src_compile" "apache1_src_install"
         "apache2_pkg_postinst")
        ("pam"
         "dopamd" "newpamd" "dopamsecurity" "newpamsecurity" "getpam_mod_dir"
         "dopammod" "newpammod" "pamd_mimic_system")
        ("virtualx"
         "virtualmake" "Xmake" "Xemake" "Xeconf")
        ("gnome2"
         "gnome2_src_configure" "gnome2_src_compile"
         "gnome2_src_install" "gnome2_gconf_install"
         "gnome2_gconf_uninstal" "gnome2_omf_fix"
         "gnome2_scrollkeeper_update" "gnome2_pkg_postinst"
         "gnome2_pkg_postrm")))

;; TODO EXPORT_FUNCTIONS

(add-to-list 'sh-ancestor-alist '(ebuild . bash))
(add-to-list 'sh-builtins
             `(ebuild sh-append bash ,@ebuild-core-keywords))

(define-derived-mode ebuild-mode sh-mode "ebuild"
  "Major mode for ebuilds and eclasses.

This is like shell-script-mode but enforces indentation rules and
adds some keybindings."
  ;; ebuild mode
  (sh-set-shell "ebuild")
  ;; enforce 4-space tabs
  (setq tab-width 4)
  (setq indent-tabs-mode t))

;; keybindings for ebuild-mode

; macro abuse!

; Writing this with a mapc and a bunch of lambdas did not work: the
; lambda we end up binding to the key does not bind to the "command"
; string (gotta love dynamic scoping). So we unroll the entire thing:
; this macro expands to a progn of all the define-key calls we need
; with a separate lambda per define-key, and evaluating *that* works.
(defmacro ebuild-setkeys-macro (keys)
  (cons 'progn
        (mapcar
         (lambda (pair)
            ; construct a define-key call with the key and
            ; commandstring evaluated
            `(define-key ebuild-mode-map ,(concat "\C-c\C-e" (car pair))
               (defun ,(intern (concat "ebuild-run-ebuild-" (cdr pair))) ()
                 ,(format "Run \"ebuild /path/to/ebuild %s\"" (cdr pair))
                 (interactive)
                 (ebuild-run-ebuild ,(cdr pair)))))
          keys)))

(ebuild-setkeys-macro (("l" . "clean")
                       ("c" . "compile")
                       ("d" . "digest")
                       ("f" . "fetch")
                       ("i" . "install")
                       ("m" . "merge")
                       ("o" . "postinst")
                       ("p" . "preinst")
                       ("q" . "qmerge")
                       ("t" . "test")
                       ("u" . "unpack")))

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . ebuild-mode))

(provide 'ebuild-mode)
