;;; ebuild-mode-keywords.el

;; Copyright 2006-2012 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: languages

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The commands have been grouped into lists of source (mainly eclass).
;; We map each set of keywords to the basic faces: font-lock-*-face.

;;; Code:

(defvar ebuild-mode-keywords-0
  '(("best_version" "check_KV" "die" "diropts" "dobin" "docinto" "doconfd"
     "dodir" "dodoc" "doenvd" "doexe" "dohtml" "doinfo" "doinitd"
     "doins" "dojar" "dolib" "dolib.a" "dolib.so" "doman" "domo" "dopython"
     "dosbin" "dosym" "ebegin" "econf" "eend" "eerror" "einfo"
     "einfon" "einstall" "elog" "emake" "ewarn" "exeinto" "exeopts" "fowners"
     "fperms" "has" "has_version" "hasq" "hasv" "insinto" "insopts" "into"
     "keepdir" "libopts" "newbin" "newconfd" "newdoc" "newenvd" "newexe"
     "newinitd" "newins" "newlib.a" "newlib.so" "newman" "newsbin" "prepall"
     "prepallinfo" "prepallman" "prepallstrip" "unpack" "use"
     "use_enable" "use_with" "useq" "usev")
    font-lock-type-face))

(defvar ebuild-mode-keywords-eapi4
  '(("docompress" "nonfatal")
    font-lock-type-face))

(defvar ebuild-mode-keywords-eapi5
  '(("doheader" "newheader" "usex")
    font-lock-type-face))

(defvar ebuild-mode-keywords-functions
  '(("pkg_nofetch" "pkg_setup" "src_unpack" "src_compile" "src_test"
     "src_install" "pkg_preinst" "pkg_postinst" "pkg_prerm" "pkg_postrm"
     "pkg_config")
    font-lock-type-face))

(defvar ebuild-mode-keywords-functions-eapi2
  '(("pkg_info" "src_prepare" "src_configure")
    font-lock-type-face))

(defvar ebuild-mode-keywords-functions-eapi4
  '(("pkg_pretend")
    font-lock-type-face))

(defvar ebuild-mode-keywords-functions-default
  '(("default" "default_pkg_nofetch" "default_src_unpack"
     "default_src_prepare" "default_src_configure" "default_src_compile"
     "default_src_test" "default_src_install")
    font-lock-type-face))

;; comment-face will always override the eclass documentation strings
(defvar ebuild-mode-keywords-eclass-documentation
  '(("@AUTHOR" "@BLURB" "@CODE" "@DEFAULT_UNSET" "@DESCRIPTION" "@ECLASS"
     "@ECLASS-VARIABLE" "@EXAMPLE" "@FUNCTION" "@INTERNAL" "@MAINTAINER"
     "@REQUIRED" "@RETURN" "@ROFF" "@USAGE" "@VARIABLE")
    font-lock-type-face))

(defvar ebuild-mode-keywords-warn
  ;; warn about "which" usage
  ;; see http://permalink.gmane.org/gmane.linux.gentoo.devel/46770
  '(("which" "EAPI" "bindnow-flags" "has_m64" "has_m32")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-deprecated
  ;; deprecated eclass functions
  '(("elisp-comp" "prepalldocs" "dosed" "dohard" "python_mod_compile"
     "dobashcompletion" "bash-completion_pkg_postinst" "qt4_min_version"
     "qt4_min_version_list")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-sandbox
  '(("adddeny" "addpredict" "addread" "addwrite")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-eclass
  '(("inherit")
    font-lock-type-face))

;; All keyword lists below this line are auto-generated
;; from keyword-generation.sh

(defvar ebuild-mode-keywords-alternatives
  '(("alternatives_auto_makesym" "alternatives_makesym"
     "alternatives_pkg_postinst" "alternatives_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ant-tasks
  '(("ant-tasks_src_compile" "ant-tasks_src_install" "ant-tasks_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-apache-2
  '(("apache-2_pkg_postinst" "apache-2_pkg_setup" "apache-2_src_configure"
     "apache-2_src_install" "apache-2_src_prepare" "check_module_critical"
     "check_module_depends" "check_upgrade" "generate_load_module"
     "setup_modules" "setup_mpm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-apache-module
  '(("apache-module_pkg_postinst" "apache-module_src_compile"
     "apache-module_src_install" "apache_cd_dir" "apache_doc_magic"
     "apache_mod_file")
    font-lock-type-face))

(defvar ebuild-mode-keywords-aspell-dict
  '(("aspell-dict_src_compile" "aspell-dict_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-autotools
  '(("autotools_check_macro" "autotools_check_macro_val" "autotools_env_setup"
     "autotools_m4dir_include" "autotools_m4sysdir_include"
     "autotools_run_tool" "config_rpath_update" "eaclocal" "eaclocal_amflags"
     "eautoconf" "eautoheader" "eautomake" "eautopoint" "eautoreconf")
    font-lock-type-face))

(defvar ebuild-mode-keywords-autotools-utils
  '(("autotools-utils_autoreconf" "autotools-utils_src_compile"
     "autotools-utils_src_configure" "autotools-utils_src_install"
     "autotools-utils_src_prepare" "autotools-utils_src_test"
     "remove_libtool_files")
    font-lock-type-face))

(defvar ebuild-mode-keywords-base
  '(("base_src_compile" "base_src_configure" "base_src_install"
     "base_src_install_docs" "base_src_make" "base_src_prepare"
     "base_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bash-completion-r1
  '(("dobashcomp" "newbashcomp")
    font-lock-type-face))

(defvar ebuild-mode-keywords-boost-utils
  '(("boost-utils_export_root" "boost-utils_get_best_slot"
     "boost-utils_get_includedir" "boost-utils_get_libdir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bsdmk
  '(("append-opt" "bsdmk_src_compile" "bsdmk_src_install" "dummy_mk"
     "mkinstall" "mkmake")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bzr
  '(("bzr_bootstrap" "bzr_fetch" "bzr_initial_fetch" "bzr_src_prepare"
     "bzr_src_unpack" "bzr_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cannadic
  '(("cannadic-install" "cannadic_pkg_postinst" "cannadic_pkg_postrm"
     "cannadic_pkg_setup" "cannadic_src_install" "dicsdir-install"
     "update-cannadic-dir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cdrom
  '(("cdrom_get_cds" "cdrom_load_next_cd")
    font-lock-type-face))

(defvar ebuild-mode-keywords-check-reqs
  '(("check-reqs_disk" "check-reqs_get_mebibytes" "check-reqs_get_number"
     "check-reqs_get_unit" "check-reqs_memory" "check-reqs_output"
     "check-reqs_pkg_pretend" "check-reqs_pkg_setup" "check-reqs_prepare"
     "check-reqs_run" "check-reqs_start_phase" "check-reqs_unsatisfied"
     "check_reqs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-chromium
  '(("chromium_bundled_v8_version" "chromium_installed_v8_version"
     "chromium_pkg_die" "chromium_pkg_postinst" "chromium_pkg_postrm"
     "chromium_pkg_preinst" "chromium_remove_language_paks"
     "chromium_suid_sandbox_check_kernel_config" "egyp_chromium" "gyp_use")
    font-lock-type-face))

(defvar ebuild-mode-keywords-clutter
  '(("clutter_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cmake-utils
  '(("cmake-utils_src_compile" "cmake-utils_src_configure"
     "cmake-utils_src_install" "cmake-utils_src_make" "cmake-utils_src_test"
     "cmake-utils_use" "cmake-utils_use_build" "cmake-utils_use_disable"
     "cmake-utils_use_enable" "cmake-utils_use_has" "cmake-utils_use_no"
     "cmake-utils_use_use" "cmake-utils_use_want" "cmake-utils_use_with"
     "enable_cmake-utils_src_compile" "enable_cmake-utils_src_configure"
     "enable_cmake-utils_src_install" "enable_cmake-utils_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common-2
  '(("do-debian-credits" "impl-remove-timestamp-hack"
     "impl-restore-timestamp-hack" "impl-save-timestamp-hack"
     "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common-3
  '(("do-debian-credits" "impl-remove-timestamp-hack"
     "impl-restore-timestamp-hack" "impl-save-timestamp-hack"
     "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common
  '(("do-debian-credits" "impl-remove-timestamp-hack"
     "impl-restore-timestamp-hack" "impl-save-timestamp-hack"
     "register-common-lisp-implementation"
     "reregister-all-common-lisp-implementations" "standard-impl-postinst"
     "standard-impl-postrm" "test-in" "unregister-common-lisp-implementation")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp
  '(("common-lisp-install" "common-lisp-system-symlink"
     "common-lisp_pkg_postinst" "common-lisp_pkg_postrm"
     "common-lisp_pkg_preinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-confutils
  '(("confutils_init" "confutils_require_any"
     "confutils_require_built_with_all" "confutils_require_built_with_any"
     "confutils_require_one" "confutils_use_conflict"
     "confutils_use_depend_all" "confutils_use_depend_any"
     "confutils_use_depend_built_with_all"
     "confutils_use_depend_built_with_any" "enable_extension_disable"
     "enable_extension_enable" "enable_extension_enable_built_with"
     "enable_extension_enableonly" "enable_extension_with"
     "enable_extension_with_built_with" "enable_extension_withonly"
     "enable_extension_without")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cron
  '(("cron_pkg_postinst" "docron" "docrondir" "docrontab")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cvs
  '(("cvs_fetch" "cvs_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-darcs
  '(("darcs_fetch" "darcs_patchcount" "darcs_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-db
  '(("db_fix_so" "db_src_install_doc" "db_src_install_examples"
     "db_src_install_headerslot" "db_src_install_usrbinslot"
     "db_src_install_usrlibcleanup" "db_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-db-use
  '(("db_findver" "db_includedir" "db_libname" "db_ver_to_slot")
    font-lock-type-face))

(defvar ebuild-mode-keywords-depend.apache
  '(("depend.apache_pkg_setup" "has_apache" "has_apache_threads"
     "has_apache_threads_in" "need_apache" "need_apache2" "need_apache2_2"
     "need_apache2_4" "want_apache" "want_apache2" "want_apache2_2")
    font-lock-type-face))

(defvar ebuild-mode-keywords-depend.php
  '(("dodoc-php" "dohtml-php" "has_concurrentmodphp" "has_debug" "has_php"
     "has_zts" "need_php" "need_php5" "need_php5_cli" "need_php5_httpd"
     "need_php_by_category" "need_php_cli" "need_php_httpd"
     "php_binary_extension" "require_gd" "require_pdo" "require_php_cgi"
     "require_php_cli" "require_php_sapi_from" "require_php_with_any_use"
     "require_php_with_use" "require_sqlite" "uses_php5")
    font-lock-type-face))

(defvar ebuild-mode-keywords-distutils
  '(("distutils_get_intermediate_installation_image" "distutils_pkg_postinst"
     "distutils_pkg_postrm" "distutils_src_compile" "distutils_src_install"
     "distutils_src_prepare" "distutils_src_test" "distutils_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-elisp-common
  '(("elisp-compile" "elisp-emacs-version" "elisp-install"
     "elisp-make-autoload-file" "elisp-need-emacs" "elisp-site-file-install"
     "elisp-site-regen")
    font-lock-type-face))

(defvar ebuild-mode-keywords-elisp
  '(("elisp_pkg_postinst" "elisp_pkg_postrm" "elisp_pkg_setup"
     "elisp_src_compile" "elisp_src_configure" "elisp_src_install"
     "elisp_src_prepare" "elisp_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-embassy
  '(("embassy_src_compile" "embassy_src_install" "embassy_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-emboss
  '(("emboss_src_configure" "emboss_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-emul-linux-x86
  '(("emul-linux-x86_src_install" "emul-linux-x86_src_prepare"
     "emul-linux-x86_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-enlightenment
  '(("enlightenment_src_compile" "enlightenment_src_configure"
     "enlightenment_src_install" "enlightenment_src_prepare"
     "enlightenment_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-eutils
  '(("built_with_use" "check_license" "doicon" "domenu" "ebeep" "ecvs_clean"
     "edos2unix" "emktemp" "epatch" "epatch_user" "epause" "epunt_cxx"
     "eqawarn" "eshopts_pop" "eshopts_push" "estack_pop" "estack_push"
     "esvn_clean" "eumask_pop" "eumask_push" "in_iuse" "make_desktop_entry"
     "make_session_desktop" "make_wrapper" "newicon" "newmenu" "path_exists"
     "preserve_old_lib" "preserve_old_lib_notify" "prune_libtool_files"
     "strip-linguas" "use_if_iuse" "usex" "validate_desktop_entries")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fdo-mime
  '(("fdo-mime_desktop_database_update" "fdo-mime_mime_database_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-findlib
  '(("check_ocamlfind" "findlib_src_install" "findlib_src_preinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fixheadtails
  '(("ht_fix_all" "ht_fix_file")
    font-lock-type-face))

(defvar ebuild-mode-keywords-flag-o-matic
  '(("all-flag-vars" "append-cflags" "append-cppflags" "append-cxxflags"
     "append-fflags" "append-flags" "append-ldflags" "append-lfs-flags"
     "append-libs" "filter-flags" "filter-ldflags" "filter-lfs-flags"
     "filter-mfpmath" "get-flag" "is-flag" "is-flagq" "is-ldflag" "is-ldflagq"
     "no-as-needed" "raw-ldflags" "replace-cpu-flags" "replace-flags"
     "replace-sparc64-flags" "setup-allowed-flags" "strip-flags"
     "strip-unsupported-flags" "test-flag-CC" "test-flag-CXX" "test-flag-F77"
     "test-flag-FC" "test-flag-PROG" "test-flags" "test-flags-CC"
     "test-flags-CXX" "test-flags-F77" "test-flags-FC" "test-flags-PROG"
     "test_version_info")
    font-lock-type-face))

(defvar ebuild-mode-keywords-font-ebdftopcf
  '(("ebdftopcf" "font-ebdftopcf_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-font
  '(("font_cleanup_dirs" "font_fontconfig" "font_pkg_postinst"
     "font_pkg_postrm" "font_pkg_setup" "font_src_install"
     "font_xfont_config")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fortran-2
  '(("fortran-2_pkg_setup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fox
  '(("fox_pkg_postinst" "fox_src_compile" "fox_src_configure"
     "fox_src_install" "fox_src_prepare" "fox_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-freebsd
  '(("doperiodic" "freebsd_do_patches" "freebsd_get_bmake"
     "freebsd_rename_libraries" "freebsd_src_compile" "freebsd_src_install"
     "freebsd_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-freedict
  '(("freedict_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games
  '(("dogamesbin" "dogameslib" "dogameslib.a" "dogameslib.so" "dogamessbin"
     "egamesconf" "games_get_libdir" "games_make_wrapper" "games_pkg_postinst"
     "games_pkg_preinst" "games_pkg_setup" "games_src_compile"
     "games_src_configure" "games_umod_unpack" "games_ut_unpack" "gamesenv"
     "gamesowners" "gamesperms" "gameswrapper" "newgamesbin" "newgamessbin"
     "prepgamesdirs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games-ggz
  '(("games-ggz_pkg_postinst" "games-ggz_pkg_postrm" "games-ggz_src_compile"
     "games-ggz_src_configure" "games-ggz_src_install"
     "games-ggz_update_modules")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games-mods
  '(("games-mods_dosyms" "games-mods_get_rdepend" "games-mods_make_confd"
     "games-mods_make_initd" "games-mods_pkg_postinst"
     "games-mods_src_install" "games-mods_use_dedicated"
     "games-mods_use_opengl")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gdesklets
  '(("gdesklets_pkg_postinst" "gdesklets_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ghc-package
  '(("ghc-bestcabalversion" "ghc-confdir" "ghc-elem"
     "ghc-extractportageversion" "ghc-fixlibpath" "ghc-getghc" "ghc-getghcpkg"
     "ghc-getghcpkgbin" "ghc-install-pkg" "ghc-libdir" "ghc-listpkg"
     "ghc-localpkgconf" "ghc-makeghcilib" "ghc-package-exists"
     "ghc-package_pkg_postinst" "ghc-package_pkg_prerm" "ghc-register-pkg"
     "ghc-reregister" "ghc-reverse" "ghc-sanecabal" "ghc-setup-pkg"
     "ghc-supports-shared-libraries" "ghc-unregister-pkg" "ghc-version")
    font-lock-type-face))

(defvar ebuild-mode-keywords-git-2
  '(("git-2_bootstrap" "git-2_branch" "git-2_cleanup" "git-2_fetch" "git-2_gc"
     "git-2_init_variables" "git-2_initial_clone" "git-2_migrate_repository"
     "git-2_move_source" "git-2_prepare_storedir" "git-2_src_unpack"
     "git-2_submodules" "git-2_update_repo")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gkrellm-plugin
  '(("gkrellm-plugin_dir" "gkrellm-plugin_pkg_setup"
     "gkrellm-plugin_server_dir" "gkrellm-plugin_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnatbuild
  '(("add_profile_eselect_conf" "create_eselect_conf" "create_specs_file"
     "disgusting_gcc_multilib_HACK" "do_gnat_config" "gnatbuild_pkg_postinst"
     "gnatbuild_pkg_postrm" "gnatbuild_pkg_setup" "gnatbuild_src_compile"
     "gnatbuild_src_install" "gnatbuild_src_unpack" "is_crosscompile"
     "is_multilib" "should_we_eselect_gnat")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnat
  '(("belongs_to_standard" "expand_BuildEnv" "filter_env_var"
     "get_active_profile" "get_ada_dep" "get_gnat_value" "gnat_filter_flags"
     "gnat_pkg_postinst" "gnat_pkg_setup" "gnat_src_compile"
     "gnat_src_install" "lib_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome2
  '(("gnome2_pkg_postinst" "gnome2_pkg_postrm" "gnome2_pkg_preinst"
     "gnome2_src_compile" "gnome2_src_configure" "gnome2_src_install"
     "gnome2_src_prepare" "gnome2_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome2-utils
  '(("gnome2_environment_reset" "gnome2_gconf_install" "gnome2_gconf_savelist"
     "gnome2_gconf_uninstall" "gnome2_icon_cache_update"
     "gnome2_icon_savelist" "gnome2_omf_fix" "gnome2_query_immodules_gtk2"
     "gnome2_query_immodules_gtk3" "gnome2_schemas_savelist"
     "gnome2_schemas_update" "gnome2_scrollkeeper_savelist"
     "gnome2_scrollkeeper_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome-python-common
  '(("gnome-python-common_pkg_postinst" "gnome-python-common_pkg_postrm"
     "gnome-python-common_pkg_setup" "gnome-python-common_src_compile"
     "gnome-python-common_src_configure" "gnome-python-common_src_install"
     "gnome-python-common_src_prepare" "gnome-python-common_src_test"
     "gnome-python-common_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnuconfig
  '(("gnuconfig_do_update" "gnuconfig_findnewest" "gnuconfig_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnustep-base
  '(("egnustep_doc" "egnustep_env" "egnustep_install"
     "egnustep_install_config" "egnustep_make" "gnustep-base_pkg_postinst"
     "gnustep-base_pkg_setup" "gnustep-base_src_compile"
     "gnustep-base_src_configure" "gnustep-base_src_install"
     "gnustep-base_src_prepare" "gnustep-base_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-go-mono
  '(("go-mono_src_compile" "go-mono_src_configure" "go-mono_src_install"
     "go-mono_src_prepare" "go-mono_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gpe
  '(("gpe_src_compile" "gpe_src_configure" "gpe_src_install" "gpe_src_prepare"
     "gpe_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins10
  '(("gst-plugins10_find_plugin_dir"
     "gst-plugins10_remove_unversioned_binaries")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-bad
  '(("gst-plugins-bad_src_compile" "gst-plugins-bad_src_configure"
     "gst-plugins-bad_src_install" "gst-plugins-bad_src_prepare"
     "gst-plugins-bad_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-base
  '(("gst-plugins-base_src_compile" "gst-plugins-base_src_configure"
     "gst-plugins-base_src_install" "gst-plugins-base_src_prepare"
     "gst-plugins-base_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-good
  '(("gst-plugins-good_src_compile" "gst-plugins-good_src_configure"
     "gst-plugins-good_src_install" "gst-plugins-good_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-ugly
  '(("gst-plugins-ugly_src_compile" "gst-plugins-ugly_src_configure"
     "gst-plugins-ugly_src_install" "gst-plugins-ugly_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gtk-sharp-module
  '(("ac_path_prog_override" "add_bdepend" "add_depend" "add_rdepend"
     "get_sharp_apis" "get_sharp_assemblies"
     "gnome-sharp-tarball-post_src_prepare"
     "gnome-sharp-tarball_src_configure" "gtk-sharp-module_src_compile"
     "gtk-sharp-module_src_configure" "gtk-sharp-module_src_install"
     "gtk-sharp-module_src_prepare" "gtk-sharp-tarball-post_src_prepare"
     "gtk-sharp-tarball_src_configure" "phase_hook"
     "pkg_check_modules_override")
    font-lock-type-face))

(defvar ebuild-mode-keywords-haskell-cabal
  '(("cabal-bootstrap" "cabal-build" "cabal-configure" "cabal-copy"
     "cabal-haddock" "cabal-hscolour" "cabal-hscolour-haddock"
     "cabal-is-dummy-lib" "cabal-mksetup" "cabal-pkg" "cabal-version"
     "cabal_flag" "cabal_src_compile" "cabal_src_configure"
     "cabal_src_install" "haskell-cabal_pkg_setup" "haskell-cabal_src_compile"
     "haskell-cabal_src_configure" "haskell-cabal_src_install"
     "haskell-cabal_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-horde
  '(("horde_pkg_postinst" "horde_pkg_setup" "horde_src_install"
     "horde_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-intel-sdp
  '(("intel-sdp_pkg_postinst" "intel-sdp_pkg_postrm" "intel-sdp_pkg_pretend"
     "intel-sdp_pkg_setup" "intel-sdp_src_install" "intel-sdp_src_unpack"
     "intel_link_eclipse_plugins")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-ant-2
  '(("java-ant-2_src_configure" "java-ant_bsfix" "java-ant_bsfix_files"
     "java-ant_bsfix_one" "java-ant_ignore-system-classes"
     "java-ant_remove-taskdefs" "java-ant_rewrite-bootclasspath"
     "java-ant_rewrite-classpath" "java-ant_xml-rewrite")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-osgi
  '(("java-osgi_dojar" "java-osgi_dojar-fromfile" "java-osgi_newjar"
     "java-osgi_newjar-fromfile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-pkg-2
  '(("java-pkg-2_pkg_preinst" "java-pkg-2_pkg_setup" "java-pkg-2_src_compile"
     "java-pkg-2_src_prepare" "java-pkg-2_src_test"
     "java-pkg-2_supports-test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-pkg-opt-2
  '(("java-pkg-opt-2_pkg_preinst" "java-pkg-opt-2_pkg_setup"
     "java-pkg-opt-2_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-pkg-simple
  '(("java-pkg-simple_src_compile" "java-pkg-simple_src_install"
     "java-pkg-simple_verbose-cmd")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-utils-2
  '(("depend-java-query" "eant" "ejavac" "ejunit" "ejunit4" "ejunit_"
     "increment-qa-violations" "is-java-strict" "java-pkg_addcp"
     "java-pkg_announce-qa-violation" "java-pkg_ant-tasks-depend"
     "java-pkg_append_" "java-pkg_build-vm-from-handle" "java-pkg_check-jikes"
     "java-pkg_check-phase" "java-pkg_check-versioned-jar"
     "java-pkg_current-vm-matches" "java-pkg_die" "java-pkg_do_write_"
     "java-pkg_doexamples" "java-pkg_dohtml" "java-pkg_dojar"
     "java-pkg_dojavadoc" "java-pkg_dolauncher" "java-pkg_doso"
     "java-pkg_dosrc" "java-pkg_dowar" "java-pkg_ensure-dep"
     "java-pkg_ensure-gcj" "java-pkg_ensure-no-bundled-jars"
     "java-pkg_ensure-test" "java-pkg_ensure-vm-version-eq"
     "java-pkg_ensure-vm-version-ge" "java-pkg_ensure-vm-version-sufficient"
     "java-pkg_expand_dir_" "java-pkg_filter-compiler"
     "java-pkg_find-normal-jars" "java-pkg_force-compiler"
     "java-pkg_func-exists" "java-pkg_get-bootclasspath"
     "java-pkg_get-current-vm" "java-pkg_get-javac" "java-pkg_get-jni-cflags"
     "java-pkg_get-source" "java-pkg_get-target" "java-pkg_get-vm-vendor"
     "java-pkg_get-vm-version" "java-pkg_getjar" "java-pkg_getjars"
     "java-pkg_init" "java-pkg_init-compiler_" "java-pkg_init_paths_"
     "java-pkg_is-vm-version-eq" "java-pkg_is-vm-version-ge"
     "java-pkg_is-vm-version-sufficient" "java-pkg_jar-from"
     "java-pkg_jar-list" "java-pkg_jarfrom" "java-pkg_jarinto"
     "java-pkg_javac-args" "java-pkg_needs-vm" "java-pkg_newjar"
     "java-pkg_record-jar_" "java-pkg_recordjavadoc"
     "java-pkg_register-ant-task" "java-pkg_register-dependency"
     "java-pkg_register-environment-variable"
     "java-pkg_register-optional-dependency" "java-pkg_regjar"
     "java-pkg_regso" "java-pkg_set-current-vm" "java-pkg_setup-vm"
     "java-pkg_sointo" "java-pkg_switch-vm" "java-pkg_verify-classes"
     "java-utils-2_pkg_preinst" "java-utils-2_src_prepare" "use_doc")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-virtuals-2
  '(("java-virtuals-2_do_write" "java-virtuals-2_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-vm-2
  '(("get_system_arch" "install_mozilla_plugin" "java-vm-2_pkg_postinst"
     "java-vm-2_pkg_postrm" "java-vm-2_pkg_prerm" "java-vm-2_pkg_setup"
     "java-vm_check-nsplugin" "java-vm_revdep-mask" "java-vm_sandbox-predict"
     "java-vm_set-nsplugin" "java-vm_set-pax-markings" "java_get_plugin_dir_"
     "java_mozilla_clean_" "java_set_default_vm_" "set_java_env")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-base
  '(("kde4-base_pkg_postinst" "kde4-base_pkg_postrm" "kde4-base_pkg_preinst"
     "kde4-base_pkg_setup" "kde4-base_src_compile" "kde4-base_src_configure"
     "kde4-base_src_install" "kde4-base_src_prepare" "kde4-base_src_test"
     "kde4-base_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-functions
  '(("add_blocker" "add_kdebase_dep" "buildsycoca"
     "comment_all_add_subdirectory" "enable_selected_doc_linguas"
     "enable_selected_linguas" "get_kde_version"
     "install_library_dependencies" "load_library_dependencies"
     "migrate_store_dir" "save_library_dependencies")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-meta
  '(("kde4-meta_change_cmakelists" "kde4-meta_create_extractlists"
     "kde4-meta_pkg_postinst" "kde4-meta_pkg_postrm" "kde4-meta_pkg_preinst"
     "kde4-meta_pkg_setup" "kde4-meta_src_compile" "kde4-meta_src_configure"
     "kde4-meta_src_extract" "kde4-meta_src_install" "kde4-meta_src_prepare"
     "kde4-meta_src_test" "kde4-meta_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kernel-2
  '(("compile_headers" "compile_headers_tweak_config" "cross_pre_c_headers"
     "debug-print-kernel2-variables" "detect_arch" "detect_version"
     "env_setup_xmakeopts" "getfilevar" "handle_genpatches" "headers___fix"
     "install_headers" "install_sources" "install_universal"
     "kernel-2_pkg_postinst" "kernel-2_pkg_postrm" "kernel-2_pkg_preinst"
     "kernel-2_pkg_setup" "kernel-2_src_compile" "kernel-2_src_install"
     "kernel-2_src_test" "kernel-2_src_unpack" "kernel_header_destdir"
     "kernel_is" "kernel_is_2_4" "kernel_is_2_6" "postinst_sources"
     "preinst_headers" "setup_headers" "unipatch" "universal_unpack"
     "unpack_2_4" "unpack_2_6" "unpack_fix_install_path"
     "unpack_set_extraversion")
    font-lock-type-face))

(defvar ebuild-mode-keywords-l10n
  '(("l10n_find_plocales_changes" "l10n_for_each_disabled_locale_do"
     "l10n_for_each_locale_do" "l10n_get_locales")
    font-lock-type-face))

(defvar ebuild-mode-keywords-latex-package
  '(("latex-package_has_tetex_3" "latex-package_pkg_postinst"
     "latex-package_pkg_postrm" "latex-package_rehash"
     "latex-package_src_compile" "latex-package_src_doinstall"
     "latex-package_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-libtool
  '(("ELT_libtool_version" "ELT_try_and_apply_patch" "ELT_walk_patches"
     "VER_major" "VER_micro" "VER_minor" "VER_to_int" "darwintoolize"
     "elibtoolize" "elt_patch_dir" "uclibctoolize")
    font-lock-type-face))

(defvar ebuild-mode-keywords-linux-info
  '(("check_extra_config" "check_kernel_built" "check_modules_supported"
     "check_zlibinflate" "get_localversion" "get_makefile_extract_function"
     "get_running_version" "get_version" "getfilevar" "getfilevar_noexec"
     "kernel_is" "linux-info_get_any_version" "linux-info_pkg_setup"
     "linux_chkconfig_builtin" "linux_chkconfig_module"
     "linux_chkconfig_present" "linux_chkconfig_string"
     "linux_config_bin_exists" "linux_config_exists" "linux_config_qa_check"
     "linux_config_src_exists" "qeerror" "qeinfo" "qewarn" "qout"
     "require_configured_kernel" "set_arch_to_kernel" "set_arch_to_portage")
    font-lock-type-face))

(defvar ebuild-mode-keywords-linux-mod
  '(("check_vermagic" "convert_to_m" "find_module_params" "generate_modulesd"
     "get-KERNEL_CC" "linux-mod_pkg_postinst" "linux-mod_pkg_postrm"
     "linux-mod_pkg_preinst" "linux-mod_pkg_setup"
     "linux-mod_pkg_setup_binary" "linux-mod_src_compile"
     "linux-mod_src_install" "move_old_moduledb" "remove_moduledb" "set_kvobj"
     "strip_modulenames" "update_depmod" "update_moduledb" "update_modules"
     "use_m")
    font-lock-type-face))

(defvar ebuild-mode-keywords-makeedit
  '(("edit_makefiles")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mercurial
  '(("mercurial_fetch" "mercurial_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mono
  '(("egacinstall" "mono_multilib_comply")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mount-boot
  '(("mount-boot_mount_boot_partition" "mount-boot_pkg_postinst"
     "mount-boot_pkg_postrm" "mount-boot_pkg_preinst" "mount-boot_pkg_prerm"
     "mount-boot_umount_boot_partition")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozconfig-3
  '(("mozconfig_config")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozcoreconf-2
  '(("makemake2" "moz_pkgsetup" "mozconfig_annotate" "mozconfig_final"
     "mozconfig_init" "mozconfig_use_enable" "mozconfig_use_extension"
     "mozconfig_use_with")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozextension
  '(("xpi_install" "xpi_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozilla-launcher
  '(("install_mozilla_launcher_stub" "update_mozilla_launcher_symlinks"
     "warn_mozilla_launcher_stub")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozlinguas
  '(("mozlinguas_export" "mozlinguas_src_install" "mozlinguas_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-multilib
  '(("get_abi_CFLAGS" "get_abi_CHOST" "get_abi_CTARGET" "get_abi_FAKE_TARGETS"
     "get_abi_LDFLAGS" "get_abi_LIBDIR" "get_abi_var" "get_all_abis"
     "get_all_libdirs" "get_install_abis" "get_libdir" "get_libname"
     "get_modname" "has_multilib_profile" "is_final_abi" "multilib_env"
     "multilib_toolchain_setup" "number_abis")
    font-lock-type-face))

(defvar ebuild-mode-keywords-multiprocessing
  '(("makeopts_jobs" "multijob_child_init" "multijob_finish"
     "multijob_finish_one" "multijob_init" "multijob_post_fork"
     "multijob_pre_fork" "redirect_alloc_fd")
    font-lock-type-face))

(defvar ebuild-mode-keywords-myspell
  '(("get_myspell_lang" "get_myspell_ooo_uri" "get_myspell_suffixes"
     "myspell_pkg_postinst" "myspell_pkg_preinst" "myspell_src_install"
     "set_fields")
    font-lock-type-face))

(defvar ebuild-mode-keywords-myspell-r2
  '(("myspell-r2_src_install" "myspell-r2_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql-autotools
  '(("mysql-autotools_configure_51" "mysql-autotools_configure_common"
     "mysql-autotools_configure_minimal" "mysql-autotools_disable_test"
     "mysql-autotools_src_compile" "mysql-autotools_src_configure"
     "mysql-autotools_src_install" "mysql-autotools_src_prepare"
     "pbxt_src_compile" "pbxt_src_configure" "pbxt_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql-cmake
  '(("configure_cmake_locale" "configure_cmake_minimal"
     "configure_cmake_standard" "mysql-cmake_disable_test"
     "mysql-cmake_src_compile" "mysql-cmake_src_configure"
     "mysql-cmake_src_install" "mysql-cmake_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql
  '(("configure_40_41_50" "configure_51" "configure_common"
     "configure_minimal" "mysql_disable_test" "mysql_init_vars"
     "mysql_pkg_config" "mysql_pkg_postinst" "mysql_pkg_postrm"
     "mysql_pkg_preinst" "mysql_pkg_setup" "mysql_src_compile"
     "mysql_src_configure" "mysql_src_install" "mysql_src_prepare"
     "mysql_src_unpack" "pbxt_available" "pbxt_patch_available"
     "pbxt_src_compile" "pbxt_src_configure" "pbxt_src_install"
     "xtradb_patch_available")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql_fx
  '(("mysql_check_version_range" "mysql_init_vars" "mysql_lib_symlinks"
     "mysql_mv_patches" "mysql_version_is_at_least" "stripdots")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql-v2
  '(("configure_common" "configure_minimal" "mysql-v2_disable_test"
     "mysql-v2_pkg_config" "mysql-v2_pkg_postinst" "mysql-v2_pkg_postrm"
     "mysql-v2_pkg_preinst" "mysql-v2_pkg_setup" "mysql-v2_src_compile"
     "mysql-v2_src_configure" "mysql-v2_src_install" "mysql-v2_src_prepare"
     "mysql-v2_src_unpack" "pbxt_available" "pbxt_patch_available"
     "xtradb_patch_available")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mythtv-plugins
  '(("mythtv-plugins_pkg_setup" "mythtv-plugins_src_compile"
     "mythtv-plugins_src_configure" "mythtv-plugins_src_install"
     "mythtv-plugins_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-nsplugins
  '(("inst_plugin" "pkg_mv_plugins" "share_plugins_dir" "src_mv_plugins")
    font-lock-type-face))

(defvar ebuild-mode-keywords-nvidia-driver
  '(("nvidia-driver-check-warning" "nvidia-driver-get-card"
     "nvidia-driver-get-mask")
    font-lock-type-face))

(defvar ebuild-mode-keywords-oasis
  '(("oasis_src_compile" "oasis_src_configure" "oasis_src_install"
     "oasis_src_test" "oasis_use_enable")
    font-lock-type-face))

(defvar ebuild-mode-keywords-obs-service
  '(("obs-service_src_install" "obs-service_src_prepare"
     "obs-service_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-office-ext
  '(("office-ext_add_extension" "office-ext_flush_unopkg_cache"
     "office-ext_get_implementation" "office-ext_pkg_postinst"
     "office-ext_pkg_prerm" "office-ext_remove_extension"
     "office-ext_src_install" "office-ext_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-openib
  '(("block_other_ofed_versions" "openib_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-pam
  '(("cleanpamd" "dopamd" "dopammod" "dopamsecurity" "getpam_mod_dir"
     "newpamd" "newpammod" "newpamsecurity" "pam_epam_expand" "pamd_mimic"
     "pamd_mimic_system" "pammod_hide_symbols")
    font-lock-type-face))

(defvar ebuild-mode-keywords-pax-utils
  '(("host-is-pax" "list-paxables" "pax-mark")
    font-lock-type-face))

(defvar ebuild-mode-keywords-perl-app
  '(("perl-app_src_compile" "perl-app_src_configure" "perl-app_src_prep")
    font-lock-type-face))

(defvar ebuild-mode-keywords-perl-module
  '(("fixlocalpod" "perl-module_pkg_postinst" "perl-module_pkg_postrm"
     "perl-module_pkg_preinst" "perl-module_pkg_prerm" "perl-module_pkg_setup"
     "perl-module_src_compile" "perl-module_src_configure"
     "perl-module_src_install" "perl-module_src_prep"
     "perl-module_src_prepare" "perl-module_src_test" "perl-module_src_unpack"
     "perl_delete_localpod" "perl_delete_module_manpages"
     "perl_delete_packlist" "perl_fix_osx_extra" "perl_link_duallife_scripts"
     "perl_remove_temppath" "perl_set_eprefix" "perl_set_version" "perlinfo")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-common-r1
  '(("php_check_cflags" "php_check_imap" "php_check_java" "php_check_mta"
     "php_check_oracle_8" "php_check_oracle_all" "php_check_pgsql"
     "php_get_mycnf_charset" "php_install_java" "php_install_java_inifile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-phpconfutils
  '(("phpconfutils_built_with_use" "phpconfutils_extension_disable"
     "phpconfutils_extension_enable" "phpconfutils_extension_with"
     "phpconfutils_extension_without" "phpconfutils_generate_usefile"
     "phpconfutils_init" "phpconfutils_require_any" "phpconfutils_sort_flags"
     "phpconfutils_use_conflict" "phpconfutils_use_depend_all"
     "phpconfutils_use_depend_any" "phpconfutils_usecheck"
     "phpconfutils_warn_about_external_deps")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-base-r1
  '(("php-ext-base-r1_addextension" "php-ext-base-r1_addtoinifile"
     "php-ext-base-r1_addtoinifiles" "php-ext-base-r1_buildinilist"
     "php-ext-base-r1_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-pecl-r2
  '(("php-ext-pecl-r2_src_compile" "php-ext-pecl-r2_src_install"
     "php-ext-pecl-r2_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-source-r2
  '(("php-ext-source-r2_addextension" "php-ext-source-r2_addtoinifile"
     "php-ext-source-r2_addtoinifiles" "php-ext-source-r2_buildinilist"
     "php-ext-source-r2_createinifiles" "php-ext-source-r2_phpize"
     "php-ext-source-r2_src_compile" "php-ext-source-r2_src_configure"
     "php-ext-source-r2_src_install" "php-ext-source-r2_src_prepare"
     "php-ext-source-r2_src_unpack" "php_get_slots" "php_init_slot_env")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ezc
  '(("fix_EZC_PV")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-lib-r1
  '(("php-lib-r1_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-pear-lib-r1
  '(("php-pear-lib-r1_pkg_setup" "php-pear-lib-r1_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-pear-r1
  '(("fix_PEAR_PV" "php-pear-r1_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-portability
  '(("dlopen_lib" "get_bmake" "get_mounts" "is-login-disabled" "seq"
     "treecopy")
    font-lock-type-face))

(defvar ebuild-mode-keywords-prefix
  '(("eprefixify")
    font-lock-type-face))

(defvar ebuild-mode-keywords-python-distutils-ng
  '(("python-distutils-ng_doscript" "python-distutils-ng_newscript"
     "python-distutils-ng_redoscript" "python-distutils-ng_rewrite_hashbang"
     "python-distutils-ng_src_compile" "python-distutils-ng_src_configure"
     "python-distutils-ng_src_install" "python-distutils-ng_src_prepare"
     "python-distutils-ng_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-python
  '(("PYTHON" "python_clean_installation_image"
     "python_clean_py-compile_files" "python_convert_shebangs"
     "python_copy_sources" "python_disable_pyc" "python_enable_pyc"
     "python_execute_function" "python_execute_nosetests"
     "python_execute_py.test" "python_execute_trial"
     "python_generate_wrapper_scripts" "python_get_implementation"
     "python_get_implementation_and_version"
     "python_get_implementational_package" "python_get_includedir"
     "python_get_libdir" "python_get_library" "python_get_sitedir"
     "python_get_version" "python_merge_intermediate_installation_images"
     "python_mod_cleanup" "python_mod_optimize" "python_need_rebuild"
     "python_pkg_setup" "python_set_active_version" "python_src_compile"
     "python_src_configure" "python_src_install" "python_src_prepare"
     "python_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qmail
  '(("dospp" "dosupervise" "genqmail_src_unpack" "is_prime" "primes"
     "qmail_base_install" "qmail_config_fast" "qmail_config_install"
     "qmail_config_notice" "qmail_create_groups" "qmail_create_users"
     "qmail_full_install" "qmail_maildir_install" "qmail_man_install"
     "qmail_queue_setup" "qmail_rootmail_fixup" "qmail_sendmail_install"
     "qmail_set_cc" "qmail_spp_install" "qmail_spp_src_compile"
     "qmail_spp_src_unpack" "qmail_src_compile" "qmail_src_install"
     "qmail_src_postunpack" "qmail_ssl_generate" "qmail_ssl_install"
     "qmail_supervise_config_notice" "qmail_supervise_install"
     "qmail_tcprules_build" "qmail_tcprules_config" "qmail_tcprules_fixup"
     "qmail_tcprules_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qt4-build
  '(("build_directories" "fix_includes" "fix_library_files"
     "generate_qconfigs" "install_directories" "install_qconfigs"
     "prepare_directories" "qt4-build_pkg_postinst" "qt4-build_pkg_postrm"
     "qt4-build_pkg_setup" "qt4-build_src_compile" "qt4-build_src_configure"
     "qt4-build_src_install" "qt4-build_src_prepare" "qt4-build_src_test"
     "qt4-build_src_unpack" "qt_mkspecs_dir" "qt_nolibx11" "qt_use" "setqtenv"
     "skip_project_generation" "skip_qmake_build"
     "symlink_binaries_to_buildtree")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qt4-r2
  '(("eqmake4" "qt4-r2_src_compile" "qt4-r2_src_configure"
     "qt4-r2_src_install" "qt4-r2_src_prepare" "qt4-r2_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rox-0install
  '(("0install_native_feed" "rox-0install_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rox
  '(("expandmime" "rox_install_desktop" "rox_install_wrapper"
     "rox_pkg_postinst" "rox_pkg_postrm" "rox_pkg_setup" "rox_src_compile"
     "rox_src_install" "usemime")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rpm
  '(("rpm_spec_epatch" "rpm_src_unpack" "rpm_unpack" "srcrpm_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-fakegem
  '(("all_fakegem_compile" "all_fakegem_install" "all_ruby_compile"
     "all_ruby_install" "all_ruby_unpack" "each_fakegem_install"
     "each_fakegem_test" "each_ruby_install" "each_ruby_test"
     "ruby_fakegem_binwrapper" "ruby_fakegem_doins" "ruby_fakegem_gemsdir"
     "ruby_fakegem_gemspec_gemspec" "ruby_fakegem_genspec"
     "ruby_fakegem_install_gemspec" "ruby_fakegem_metadata_gemspec"
     "ruby_fakegem_newins")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-ng
  '(("doruby" "ruby-ng_cucumber" "ruby-ng_pkg_setup" "ruby-ng_rspec"
     "ruby-ng_src_compile" "ruby-ng_src_configure" "ruby-ng_src_install"
     "ruby-ng_src_prepare" "ruby-ng_src_test" "ruby-ng_src_unpack"
     "ruby-ng_testrb-2" "ruby_add_bdepend" "ruby_add_rdepend"
     "ruby_get_hdrdir" "ruby_get_implementation" "ruby_get_libruby"
     "ruby_get_use_implementations" "ruby_get_use_targets" "ruby_get_version"
     "ruby_implementation_command" "ruby_implementation_depend"
     "ruby_implementations_depend" "ruby_rbconfig_value" "ruby_samelib")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-ng-gnome2
  '(("all_ruby_install" "each_ruby_compile" "each_ruby_configure"
     "each_ruby_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-savedconfig
  '(("restore_config" "save_config" "savedconfig_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-scons-utils
  '(("escons" "scons_clean_makeopts" "use_scons")
    font-lock-type-face))

(defvar ebuild-mode-keywords-scsh
  '(("scsh_get_layout_conf" "scsh_scsh_path" "scsh_src_compile"
     "scsh_src_install" "scsh_src_unpack" "set_layout" "set_path_variables")
    font-lock-type-face))

(defvar ebuild-mode-keywords-selinux-policy-2
  '(("selinux-policy-2_pkg_postinst" "selinux-policy-2_src_compile"
     "selinux-policy-2_src_install" "selinux-policy-2_src_prepare"
     "selinux-policy-2_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-sgml-catalog
  '(("sgml-catalog_cat_doinstall" "sgml-catalog_cat_doremove"
     "sgml-catalog_cat_include" "sgml-catalog_cleanup"
     "sgml-catalog_pkg_postinst" "sgml-catalog_pkg_postrm"
     "sgml-catalog_pkg_prerm" "sgml-catalog_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ssl-cert
  '(("docert" "gen_cnf" "gen_crt" "gen_csr" "gen_key" "gen_pem" "get_base"
     "install_cert")
    font-lock-type-face))

(defvar ebuild-mode-keywords-stardict
  '(("stardict_src_compile" "stardict_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-subversion
  '(("subversion__get_peg_revision" "subversion__get_repository_uri"
     "subversion__get_wc_path" "subversion__svn_info" "subversion_bootstrap"
     "subversion_fetch" "subversion_pkg_preinst" "subversion_src_prepare"
     "subversion_src_unpack" "subversion_wc_info")
    font-lock-type-face))

(defvar ebuild-mode-keywords-sword-module
  '(("sword-module_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-systemd
  '(("systemd_dotmpfilesd" "systemd_dounit" "systemd_enable_service"
     "systemd_get_unitdir" "systemd_newtmpfilesd" "systemd_newunit"
     "systemd_to_myeconfargs" "systemd_with_unitdir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-texlive-common
  '(("dobin_texmf_scripts" "etexlinks" "etexmf-update"
     "texlive-common_do_symlinks" "texlive-common_handle_config_files"
     "texlive-common_is_file_present_in_texmf")
    font-lock-type-face))

(defvar ebuild-mode-keywords-texlive-module
  '(("texlive-module_add_format" "texlive-module_make_language_dat_lines"
     "texlive-module_make_language_def_lines"
     "texlive-module_make_language_lua_lines" "texlive-module_pkg_postinst"
     "texlive-module_pkg_postrm" "texlive-module_src_compile"
     "texlive-module_src_install" "texlive-module_src_unpack"
     "texlive-module_synonyms_to_language_lua_line")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain-binutils
  '(("add_src_uri" "is_cross" "tc-binutils_apply_patches" "tc-binutils_unpack"
     "toolchain-binutils_pkg_postinst" "toolchain-binutils_pkg_postrm"
     "toolchain-binutils_src_compile" "toolchain-binutils_src_install"
     "toolchain-binutils_src_test" "toolchain-binutils_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain
  '(("XGCC" "copy_minispecs_gcc_specs" "create_gcc_env_entry"
     "disable_multilib_libjava" "do_gcc_HTB_patches" "do_gcc_PIE_patches"
     "do_gcc_config" "do_gcc_rename_java_bins" "fix_libtool_libdir_paths"
     "gcc-abi-map" "gcc-compiler-configure" "gcc-lang-supported"
     "gcc-multilib-configure" "gcc_do_configure" "gcc_do_filter_flags"
     "gcc_do_make" "gcc_movelibs" "gcc_pkg_setup" "gcc_quick_unpack"
     "gcc_slot_java" "gcc_src_compile" "gcc_src_test" "gcc_src_unpack"
     "gcc_version_patch" "gentoo_urls" "get_gcc_src_uri" "get_make_var"
     "guess_patch_type_in_dir" "hardened_gcc_is_stable" "hardened_gcc_works"
     "is_ada" "is_crosscompile" "is_cxx" "is_d" "is_f77" "is_f95" "is_fortran"
     "is_gcj" "is_go" "is_multilib" "is_objc" "is_objcxx" "is_treelang"
     "make_gcc_hard" "setup_minispecs_gcc_build_specs"
     "setup_multilib_osdirnames" "should_we_gcc_config"
     "tc_version_is_at_least" "toolchain_death_notice"
     "toolchain_pkg_postinst" "toolchain_pkg_postrm" "toolchain_pkg_setup"
     "toolchain_src_compile" "toolchain_src_install" "toolchain_src_test"
     "toolchain_src_unpack" "want_minispecs" "want_pie")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain-funcs
  '(("econf_build" "gcc-fullversion" "gcc-major-version" "gcc-micro-version"
     "gcc-minor-version" "gcc-specs-directive" "gcc-specs-nostrict"
     "gcc-specs-now" "gcc-specs-pie" "gcc-specs-relro" "gcc-specs-ssp"
     "gcc-specs-ssp-to-all" "gcc-version" "gen_usr_ldscript" "tc-arch"
     "tc-arch-kernel" "tc-endian" "tc-env_build" "tc-export"
     "tc-export_build_env" "tc-getAR" "tc-getAS" "tc-getBUILD_AR"
     "tc-getBUILD_AS" "tc-getBUILD_CC" "tc-getBUILD_CPP" "tc-getBUILD_CXX"
     "tc-getBUILD_LD" "tc-getBUILD_NM" "tc-getBUILD_OBJCOPY"
     "tc-getBUILD_PKG_CONFIG" "tc-getBUILD_PROG" "tc-getBUILD_RANLIB"
     "tc-getBUILD_STRIP" "tc-getCC" "tc-getCPP" "tc-getCXX" "tc-getDLLWRAP"
     "tc-getF77" "tc-getFC" "tc-getGCJ" "tc-getLD" "tc-getNM" "tc-getOBJCOPY"
     "tc-getPKG_CONFIG" "tc-getPROG" "tc-getRANLIB" "tc-getRC" "tc-getSTRIP"
     "tc-has-openmp" "tc-has-tls" "tc-is-cross-compiler" "tc-is-softfloat"
     "tc-is-static-only" "tc-ninja_magic_to_arch")
    font-lock-type-face))

(defvar ebuild-mode-keywords-twisted
  '(("twisted_pkg_postinst" "twisted_pkg_postrm" "twisted_src_install"
     "twisted_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-unpacker
  '(("find_unpackable_file" "unpack_banner" "unpack_cpio" "unpack_deb"
     "unpack_makeself" "unpack_pdv" "unpacker" "unpacker_src_unpack"
     "unpacker_src_uri_depends")
    font-lock-type-face))

(defvar ebuild-mode-keywords-user
  '(("egetent" "egethome" "egetshell" "enewgroup" "enewuser" "esethome")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vala
  '(("vala_api_versions" "vala_best_api_version" "vala_depend"
     "vala_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vcs-snapshot
  '(("vcs-snapshot_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vdr-plugin-2
  '(("create_header_checksum_file" "create_plugindb_file"
     "delete_orphan_plugindb_file" "dev_check" "fix_vdr_libsi_include"
     "gettext_missing" "has_vdr" "linguas_support"
     "vdr-plugin-2_copy_source_tree" "vdr-plugin-2_install_source_tree"
     "vdr-plugin-2_pkg_config" "vdr-plugin-2_pkg_postinst"
     "vdr-plugin-2_pkg_postrm" "vdr-plugin-2_pkg_setup"
     "vdr-plugin-2_print_enable_command" "vdr-plugin-2_src_compile"
     "vdr-plugin-2_src_install" "vdr-plugin-2_src_prepare"
     "vdr-plugin-2_src_unpack" "vdr-plugin-2_src_util" "vdr_i18n"
     "vdr_patchmakefile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vdr-plugin
  '(("create_header_checksum_file" "create_plugindb_file"
     "delete_orphan_plugindb_file" "fix_vdr_libsi_include" "has_vdr"
     "plugin_has_gettext" "vdr-plugin_copy_source_tree"
     "vdr-plugin_install_source_tree" "vdr-plugin_pkg_config"
     "vdr-plugin_pkg_postinst" "vdr-plugin_pkg_postrm" "vdr-plugin_pkg_setup"
     "vdr-plugin_print_enable_command" "vdr-plugin_src_compile"
     "vdr-plugin_src_install" "vdr-plugin_src_prepare" "vdr-plugin_src_unpack"
     "vdr-plugin_src_util" "vdr_has_gettext" "vdr_i18n"
     "vdr_i18n_convert_to_gettext" "vdr_i18n_disable_gettext"
     "vdr_patchmakefile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-versionator
  '(("delete_all_version_separators" "delete_version_separator"
     "get_after_major_version" "get_all_version_components"
     "get_last_version_component_index" "get_major_version"
     "get_version_component_count" "get_version_component_range"
     "get_version_components" "replace_all_version_separators"
     "replace_version_separator" "version_compare" "version_format_string"
     "version_is_at_least" "version_sort")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-doc
  '(("update_vim_helptags")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim
  '(("apply_vim_patches" "update_vim_symlinks" "vim_pkg_postinst"
     "vim_pkg_postrm" "vim_pkg_setup" "vim_src_compile" "vim_src_configure"
     "vim_src_install" "vim_src_prepare" "vim_src_test" "vim_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-plugin
  '(("display_vim_plugin_help" "update_vim_afterscripts"
     "vim-plugin_pkg_postinst" "vim-plugin_pkg_postrm"
     "vim-plugin_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-spell
  '(("vim-spell_pkg_postinst" "vim-spell_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-virtualx
  '(("Xeconf" "Xemake" "Xmake" "virtualmake")
    font-lock-type-face))

(defvar ebuild-mode-keywords-virtuoso
  '(("virtuoso_src_compile" "virtuoso_src_configure" "virtuoso_src_install"
     "virtuoso_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vmware-bundle
  '(("vmware-bundle_extract-bundle-component"
     "vmware-bundle_extract-component")
    font-lock-type-face))

(defvar ebuild-mode-keywords-waf-utils
  '(("waf-utils_src_compile" "waf-utils_src_configure"
     "waf-utils_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-webapp
  '(("need_httpd" "need_httpd_cgi" "need_httpd_fastcgi"
     "webapp_check_installedat" "webapp_checkfileexists" "webapp_configfile"
     "webapp_getinstalltype" "webapp_hook_script" "webapp_pkg_postinst"
     "webapp_pkg_prerm" "webapp_pkg_setup" "webapp_postinst_txt"
     "webapp_postupgrade_txt" "webapp_read_config" "webapp_server_configfile"
     "webapp_serverowned" "webapp_sqlscript" "webapp_src_install"
     "webapp_src_preinst" "webapp_strip_appdir" "webapp_strip_cwd"
     "webapp_strip_d")
    font-lock-type-face))

(defvar ebuild-mode-keywords-wxwidgets
  '(("check_wxuse" "need-wxwidgets" "wxwidgets_pkg_setup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xemacs-elisp-common
  '(("xemacs-elisp-comp" "xemacs-elisp-compile" "xemacs-elisp-install"
     "xemacs-elisp-make-autoload-file" "xemacs-elisp-site-file-install"
     "xemacs-elisp-site-regen")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xemacs-elisp
  '(("xemacs-elisp_src_compile" "xemacs-elisp_src_install"
     "xemacs-elisp_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xemacs-packages
  '(("xemacs-packages_src_compile" "xemacs-packages_src_install"
     "xemacs-packages_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xfconf
  '(("xfconf_pkg_postinst" "xfconf_pkg_postrm" "xfconf_pkg_preinst"
     "xfconf_src_configure" "xfconf_src_install" "xfconf_src_prepare"
     "xfconf_src_unpack" "xfconf_use_debug")
    font-lock-type-face))

(defvar ebuild-mode-keywords-x-modular
  '(("create_font_cache" "create_fonts_dir" "create_fonts_scale"
     "discover_font_dirs" "install_driver_hwdata" "remove_font_metadata"
     "setup_fonts" "x-modular_debug_setup" "x-modular_dri_check"
     "x-modular_font_configure" "x-modular_patch_source"
     "x-modular_pkg_postinst" "x-modular_pkg_postrm" "x-modular_pkg_preinst"
     "x-modular_reconf_source" "x-modular_server_supports_drivers_check"
     "x-modular_specs_check" "x-modular_src_compile" "x-modular_src_configure"
     "x-modular_src_install" "x-modular_src_make" "x-modular_src_prepare"
     "x-modular_src_unpack" "x-modular_unpack_source")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xorg-2
  '(("create_fonts_dir" "create_fonts_scale" "remove_font_metadata"
     "xorg-2_flags_setup" "xorg-2_font_configure" "xorg-2_patch_source"
     "xorg-2_pkg_postinst" "xorg-2_pkg_postrm" "xorg-2_pkg_setup"
     "xorg-2_reconf_source" "xorg-2_src_compile" "xorg-2_src_configure"
     "xorg-2_src_install" "xorg-2_src_prepare" "xorg-2_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-zproduct
  '(("docs_move" "zproduct_pkg_config" "zproduct_pkg_postinst"
     "zproduct_pkg_prerm" "zproduct_src_install")
    font-lock-type-face))

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; no-byte-compile: t
;; End:

;;; ebuild-mode-keywords.el ends here
