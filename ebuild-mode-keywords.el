;;; ebuild-mode-keywords.el

;; Copyright 2006-2012 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <fauli@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: languages

;; $Id$

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
  '(("default_pkg_nofetch" "default_src_unpack" "default_src_prepare"
     "default_src_configure" "default_src_compile" "default_src_test"
     "default_src_install")
    font-lock-type-face))

;; comment-face will always override the eclass documentation strings
(defvar ebuild-mode-keywords-eclass-documentation
  '(("@BLURB" "@CODE" "@DESCRIPTION" "@ECLASS-VARIABLE" "@ECLASS" "@EXAMPLE"
     "@FUNCTION" "@MAINTAINER" "@RETURN" "@USAGE" "@VARIABLE")
    font-lock-type-face))

(defvar ebuild-mode-keywords-warn
  ;; warn about "which" usage
  ;; see http://permalink.gmane.org/gmane.linux.gentoo.devel/46770
  '(("which" "EAPI" "bindnow-flags" "has_m64" "has_m32")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-deprecated
  ;; deprecated eclass functions
  '(("elisp-comp" "prepalldocs" "dosed" "dohard" "python_mod_compile" "dobashcompletion" "bash-completion_pkg_postinst" "qt4_min_version" "qt4_min_version_list")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-sandbox
  '(("adddeny" "addpredict" "addread" "addwrite")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-eclass
  '(("inherit")
    font-lock-type-face))

;; All keyword lists below this line are auto-generated from keyword-generation.sh

(defvar ebuild-mode-keywords-alternatives
  '(("alternatives_auto_makesym" "alternatives_makesym" "alternatives_pkg_postinst" "alternatives_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ant-tasks
  '(("ant-tasks_src_unpack" "ant-tasks_src_compile" "ant-tasks_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-apache-2
  '(("setup_mpm" "check_module_critical" "check_module_depends" "setup_modules" "generate_load_module" "check_upgrade" "apache-2_pkg_setup" "apache-2_src_prepare" "apache-2_src_configure" "apache-2_src_install" "apache-2_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-apache-module
  '(("apache_cd_dir" "apache_mod_file" "apache_doc_magic" "apache-module_src_compile" "apache-module_src_install" "apache-module_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-aspell-dict
  '(("aspell-dict_src_compile" "aspell-dict_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-autotools
  '(("eautoreconf" "eaclocal_amflags" "eaclocal" "_elibtoolize" "eautoheader" "eautoconf" "eautomake" "eautopoint" "autotools_env_setup" "autotools_run_tool" "autotools_check_macro" "autotools_check_macro_val" "autotools_get_subdirs" "autotools_get_auxdir" "_autotools_m4dir_include" "autotools_m4dir_include" "autotools_m4sysdir_include" "config_rpath_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-autotools-utils
  '(("_check_build_dir" "remove_libtool_files" "autotools-utils_src_prepare" "autotools-utils_src_configure" "autotools-utils_src_compile" "autotools-utils_src_install" "autotools-utils_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-base
  '(("base_src_unpack" "base_src_prepare" "base_src_configure" "base_src_compile" "base_src_make" "base_src_install" "base_src_install_docs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bash-completion-r1
  '(("dobashcomp" "newbashcomp")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bsdmk
  '(("append-opt" "mkmake" "mkinstall" "dummy_mk" "bsdmk_src_compile" "bsdmk_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bzr
  '(("bzr_initial_fetch" "bzr_update" "bzr_fetch" "bzr_bootstrap" "bzr_src_unpack" "bzr_src_prepare")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cannadic
  '(("cannadic_pkg_setup" "cannadic-install" "dicsdir-install" "cannadic_src_install" "update-cannadic-dir" "cannadic_pkg_postinst" "cannadic_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cdrom
  '(("cdrom_get_cds" "cdrom_load_next_cd" "_cdrom_locate_file_on_cd")
    font-lock-type-face))

(defvar ebuild-mode-keywords-check-reqs
  '(("check_reqs" "check-reqs_pkg_setup" "check-reqs_pkg_pretend" "check-reqs_prepare" "check-reqs_run" "check-reqs_get_mebibytes" "check-reqs_get_number" "check-reqs_get_unit" "check-reqs_output" "check-reqs_memory" "check-reqs_disk" "check-reqs_start_phase" "check-reqs_unsatisfied")
    font-lock-type-face))

(defvar ebuild-mode-keywords-clutter
  '(("clutter_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cmake-utils
  '(("_use_me_now" "_use_me_now_inverted" "_check_build_dir" "cmake-utils_use_with" "cmake-utils_use_enable" "cmake-utils_use_disable" "cmake-utils_use_no" "cmake-utils_use_want" "cmake-utils_use_build" "cmake-utils_use_has" "cmake-utils_use_use" "cmake-utils_use" "_modify-cmakelists" "enable_cmake-utils_src_configure" "enable_cmake-utils_src_compile" "cmake-utils_src_make" "enable_cmake-utils_src_install" "enable_cmake-utils_src_test" "cmake-utils_src_configure" "cmake-utils_src_compile" "cmake-utils_src_install" "cmake-utils_src_test" "_execute_optionaly")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common-2
  '(("do-debian-credits" "impl-save-timestamp-hack" "impl-restore-timestamp-hack" "impl-remove-timestamp-hack" "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common-3
  '(("do-debian-credits" "impl-save-timestamp-hack" "impl-restore-timestamp-hack" "impl-remove-timestamp-hack" "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common
  '(("do-debian-credits" "register-common-lisp-implementation" "unregister-common-lisp-implementation" "reregister-all-common-lisp-implementations" "impl-save-timestamp-hack" "impl-restore-timestamp-hack" "impl-remove-timestamp-hack" "test-in" "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp
  '(("common-lisp_pkg_postinst" "common-lisp_pkg_postrm" "common-lisp_pkg_preinst" "common-lisp-install" "common-lisp-system-symlink")
    font-lock-type-face))

(defvar ebuild-mode-keywords-confutils
  '(("confutils_init" "confutils_require_one" "confutils_require_any" "confutils_require_built_with_all" "confutils_require_built_with_any" "confutils_use_conflict" "confutils_use_depend_all" "confutils_use_depend_any" "confutils_use_depend_built_with_all" "confutils_use_depend_built_with_any" "_confutils_shared_suffix" "enable_extension_disable" "enable_extension_enable" "enable_extension_enableonly" "enable_extension_without" "enable_extension_with" "enable_extension_withonly" "enable_extension_enable_built_with" "enable_extension_with_built_with")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cron
  '(("docrondir" "docron" "docrontab" "cron_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cvs
  '(("cvs_fetch" "cvs_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-darcs
  '(("darcs_patchcount" "darcs_fetch" "darcs_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-db
  '(("db_fix_so" "db_src_install_doc" "db_src_install_examples" "db_src_install_usrbinslot" "db_src_install_headerslot" "db_src_install_usrlibcleanup" "db_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-db-use
  '(("db_ver_to_slot" "db_findver" "db_includedir" "db_libname")
    font-lock-type-face))

(defvar ebuild-mode-keywords-depend.apache
  '(("_init_apache2" "_init_no_apache" "depend.apache_pkg_setup" "want_apache" "want_apache2" "want_apache2_2" "need_apache" "need_apache2" "need_apache2_2" "has_apache" "has_apache_threads" "has_apache_threads_in")
    font-lock-type-face))

(defvar ebuild-mode-keywords-depend.php
  '(("need_php5_cli" "need_php5_httpd" "need_php5" "uses_php5" "need_php_cli" "need_php_httpd" "need_php" "need_php_by_category" "has_php" "require_php_sapi_from" "require_php_with_use" "require_php_with_any_use" "has_zts" "has_debug" "has_concurrentmodphp" "require_pdo" "require_php_cli" "require_php_cgi" "require_sqlite" "require_gd" "php_binary_extension" "dodoc-php" "dohtml-php")
    font-lock-type-face))

(defvar ebuild-mode-keywords-distutils
  '(("_distutils_get_build_dir" "_distutils_get_PYTHONPATH" "_distutils_hook" "_distutils_prepare_global_options" "_distutils_prepare_current_working_directory" "_distutils_restore_current_working_directory" "distutils_src_unpack" "distutils_src_prepare" "distutils_src_compile" "_distutils_src_test_hook" "distutils_src_test" "distutils_src_install" "distutils_pkg_postinst" "distutils_pkg_postrm" "distutils_get_intermediate_installation_image")
    font-lock-type-face))

(defvar ebuild-mode-keywords-elisp-common
  '(("elisp-emacs-version" "elisp-need-emacs" "elisp-compile" "elisp-make-autoload-file" "elisp-install" "elisp-site-file-install" "elisp-site-regen")
    font-lock-type-face))

(defvar ebuild-mode-keywords-elisp
  '(("elisp_pkg_setup" "elisp_src_unpack" "elisp_src_prepare" "elisp_src_configure" "elisp_src_compile" "elisp_src_install" "elisp_pkg_postinst" "elisp_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-embassy
  '(("embassy_src_unpack" "embassy_src_compile" "embassy_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-emboss
  '(("emboss_src_prepare" "emboss_src_configure")
    font-lock-type-face))

(defvar ebuild-mode-keywords-emul-linux-x86
  '(("emul-linux-x86_src_unpack" "emul-linux-x86_src_prepare" "emul-linux-x86_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-enlightenment
  '(("enlightenment_src_unpack" "enlightenment_src_prepare" "enlightenment_src_configure" "enlightenment_src_compile" "enlightenment_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-eutils
  '(("epause" "ebeep" "ebeep" "epause" "ecvs_clean" "esvn_clean" "estack_push" "estack_pop" "eshopts_push" "eshopts_pop" "eumask_push" "eumask_pop" "epatch" "epatch_user" "emktemp" "edos2unix" "make_desktop_entry" "validate_desktop_entries" "make_session_desktop" "domenu" "newmenu" "doicon" "newicon" "find_unpackable_file" "unpack_pdv" "unpack_makeself" "strip-linguas" "preserve_old_lib" "preserve_old_lib_notify" "built_with_use" "epunt_cxx" "make_wrapper" "path_exists" "in_iuse" "use_if_iuse" "usex" "check_license")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fdo-mime
  '(("fdo-mime_desktop_database_update" "fdo-mime_mime_database_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-findlib
  '(("check_ocamlfind" "findlib_src_preinst" "findlib_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fixheadtails
  '(("__do_sed_fix" "ht_fix_file" "ht_fix_all")
    font-lock-type-face))

(defvar ebuild-mode-keywords-flag-o-matic
  '(("setup-allowed-flags" "_filter-hardened" "_filter-var" "filter-flags" "filter-lfs-flags" "append-cppflags" "append-cflags" "append-cxxflags" "append-fflags" "append-lfs-flags" "append-flags" "replace-flags" "replace-cpu-flags" "_is_flagq" "is-flagq" "is-flag" "is-ldflagq" "is-ldflag" "filter-mfpmath" "strip-flags" "test-flag-PROG" "test-flag-CC" "test-flag-CXX" "test-flag-F77" "test-flag-FC" "test-flags-PROG" "test-flags-CC" "test-flags-CXX" "test-flags-F77" "test-flags-FC" "test-flags" "test_version_info" "strip-unsupported-flags" "get-flag" "test_flag()" "replace-sparc64-flags" "append-libs" "append-ldflags" "filter-ldflags" "raw-ldflags" "no-as-needed")
    font-lock-type-face))

(defvar ebuild-mode-keywords-font-ebdftopcf
  '(("ebdftopcf" "font-ebdftopcf_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-font
  '(("font_xfont_config" "font_fontconfig" "font_cleanup_dirs" "font_pkg_setup" "font_src_install" "font_pkg_postinst" "font_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fortran-2
  '(("_write_testsuite" "_compile_test" "_fortran-has-openmp" "_die_msg" "fortran-2_pkg_setup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fox
  '(("fox_src_unpack" "fox_src_prepare" "fox_src_configure" "fox_src_compile" "fox_src_install" "fox_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-freebsd
  '(("doperiodic" "freebsd_get_bmake" "freebsd_do_patches" "freebsd_rename_libraries" "freebsd_src_unpack" "freebsd_src_compile" "freebsd_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-freedict
  '(("freedict_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games
  '(("games_get_libdir" "egamesconf" "gameswrapper" "dogamesbin" "dogamessbin" "dogameslib" "dogameslib.a" "dogameslib.so" "newgamesbin" "newgamessbin" "games_make_wrapper" "gamesowners" "gamesperms" "prepgamesdirs" "gamesenv" "games_pkg_setup" "games_src_configure" "games_src_compile" "games_pkg_preinst" "games_pkg_postinst" "games_ut_unpack" "games_umod_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games-ggz
  '(("games-ggz_src_configure" "games-ggz_src_compile" "games-ggz_src_install" "games-ggz_update_modules" "games-ggz_pkg_postinst" "games-ggz_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games-mods
  '(("games-mods_get_rdepend" "games-mods_use_opengl" "games-mods_use_dedicated" "games-mods_dosyms" "games-mods_make_initd" "depend" "start" "stop" "games-mods_make_confd" "games-mods_src_install" "games-mods_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gdesklets
  '(("gdesklets_src_install" "gdesklets_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ghc-package
  '(("ghc-getghc" "ghc-getghcpkg" "ghc-getghcpkgbin" "ghc-version" "ghc-cabal" "ghc-bestcabalversion" "ghc-sanecabal" "ghc-saneghc" "ghc-supports-shared-libraries" "ghc-extractportageversion" "ghc-libdir" "ghc-confdir" "ghc-localpkgconf" "ghc-makeghcilib" "ghc-package-exists" "ghc-setup-pkg" "ghc-fixlibpath" "ghc-install-pkg" "ghc-register-pkg" "ghc-reregister" "ghc-unregister-pkg" "ghc-reverse" "ghc-elem" "ghc-listpkg" "ghc-package_pkg_setup" "ghc-package_pkg_postinst" "ghc-package_pkg_prerm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-git-2
  '(("git-2_init_variables" "git-2_submodules" "git-2_branch" "git-2_gc" "git-2_prepare_storedir" "git-2_move_source" "git-2_initial_clone" "git-2_update_repo" "git-2_fetch" "git-2_bootstrap" "git-2_migrate_repository" "git-2_cleanup" "git-2_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gkrellm-plugin
  '(("gkrellm-plugin_dir" "gkrellm-plugin_server_dir" "gkrellm-plugin_pkg_setup" "gkrellm-plugin_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnatbuild
  '(("is_crosscompile" "is_multilib" "create_specs_file" "add_profile_eselect_conf" "create_eselect_conf" "should_we_eselect_gnat" "do_gnat_config" "disgusting_gcc_multilib_HACK" "gnatbuild_pkg_setup" "gnatbuild_pkg_postinst" "gnatbuild_pkg_postrm" "gnatbuild_src_unpack" "gnatbuild_src_compile" "gnatbuild_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnat
  '(("expand_BuildEnv" "get_ada_dep" "belongs_to_standard" "filter_env_var" "get_gnat_value" "get_active_profile" "gnat_filter_flags" "gnat_pkg_setup" "gnat_pkg_postinst" "lib_compile" "gnat_src_compile" "gnat_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome2
  '(("gnome2_src_unpack" "gnome2_src_prepare" "gnome2_src_configure" "gnome2_src_compile" "gnome2_src_install" "gnome2_pkg_preinst" "gnome2_pkg_postinst" "gnome2_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome2-utils
  '(("gnome2_environment_reset" "gnome2_gconf_savelist" "gnome2_gconf_install" "gnome2_gconf_uninstall" "gnome2_icon_savelist" "gnome2_icon_cache_update" "gnome2_omf_fix" "gnome2_scrollkeeper_update" "gnome2_schemas_savelist" "gnome2_schemas_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome-python-common
  '(("gnome-python-common_pkg_setup" "gnome-python-common_src_unpack" "gnome-python-common_src_prepare" "gnome-python-common_src_configure" "gnome-python-common_src_compile" "gnome-python-common_src_test" "gnome-python-common_src_install" "gnome-python-common_pkg_postinst" "gnome-python-common_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnuconfig
  '(("gnuconfig_update" "gnuconfig_do_update" "gnuconfig_findnewest")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnustep-base
  '(("gnustep-base_pkg_setup" "gnustep-base_src_unpack" "gnustep-base_src_prepare" "gnustep-base_src_configure" "gnustep-base_src_compile" "gnustep-base_src_install" "gnustep-base_pkg_postinst" "egnustep_env" "egnustep_make" "egnustep_install" "egnustep_doc" "egnustep_install_config" "gnustep_append_default" "gnustep_set_default")
    font-lock-type-face))

(defvar ebuild-mode-keywords-go-mono
  '(("go-mono_src_unpack" "go-mono_src_prepare" "go-mono_src_configure" "go-mono_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gpe
  '(("gpe_src_unpack" "gpe_src_prepare" "gpe_src_configure" "gpe_src_compile" "gpe_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins10
  '(("gst-plugins10_find_plugin_dir" "gst-plugins10_remove_unversioned_binaries")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-bad
  '(("gst-plugins-bad_src_unpack" "gst-plugins-bad_src_configure" "gst-plugins-bad_src_compile" "gst-plugins-bad_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-base
  '(("gst-plugins-base_src_configure" "gst-plugins-base_src_unpack" "gst-plugins-base_src_prepare" "gst-plugins-base_src_compile" "gst-plugins-base_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-good
  '(("gst-plugins-good_src_configure" "gst-plugins-good_src_unpack" "gst-plugins-good_src_compile" "gst-plugins-good_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins-ugly
  '(("gst-plugins-ugly_src_configure" "gst-plugins-ugly_src_unpack" "gst-plugins-ugly_src_compile" "gst-plugins-ugly_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gtk-sharp-module
  '(("add_bdepend" "add_rdepend" "add_depend" "get_sharp_apis" "get_sharp_assemblies" "phase_hook" "ac_path_prog_override" "pkg_check_modules_override" "gtk-sharp-tarball-post_src_prepare" "gnome-sharp-tarball-post_src_prepare" "gtk-sharp-module_src_prepare" "gtk-sharp-tarball_src_configure" "gnome-sharp-tarball_src_configure" "gtk-sharp-module_src_configure" "gtk-sharp-module_src_compile" "gtk-sharp-module_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-haskell-cabal
  '(("cabal-version" "cabal-bootstrap" "cabal-mksetup" "cabal-hscolour" "cabal-haddock" "cabal-hscolour-haddock" "cabal-configure" "cabal-build" "cabal-copy" "cabal-pkg" "cabal-is-dummy-lib" "haskell-cabal_pkg_setup" "haskell-cabal_src_configure" "cabal_src_configure" "cabal_src_compile" "haskell-cabal_src_compile" "haskell-cabal_src_test" "cabal_src_install" "haskell-cabal_src_install" "cabal_flag")
    font-lock-type-face))

(defvar ebuild-mode-keywords-horde
  '(("horde_pkg_setup" "horde_src_unpack" "horde_src_install" "horde_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-ant-2
  '(("java-ant-2_src_configure" "java-ant_bsfix" "_bsfix_die" "java-ant_bsfix_files" "java-ant_bsfix_one" "java-ant_rewrite-classpath" "java-ant_remove-taskdefs" "f.close" "java-ant_ignore-system-classes" "java-ant_xml-rewrite" "java-ant_rewrite-bootclasspath")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-osgi
  '(("_canonicalise" "_java-osgi_plugin" "_java-osgi_makejar" "java-osgi_dojar" "java-osgi_newjar" "_java-osgi_makejar-fromfile" "java-osgi_newjar-fromfile" "java-osgi_dojar-fromfile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-pkg-2
  '(("java-pkg-2_pkg_setup" "java-pkg-2_src_prepare" "java-pkg-2_src_compile" "java-pkg-2_supports-test" "java-pkg-2_src_test" "java-pkg-2_pkg_preinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-pkg-opt-2
  '(("java-pkg-opt-2_pkg_setup" "java-pkg-opt-2_src_prepare" "java-pkg-opt-2_pkg_preinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-pkg-simple
  '(("java-pkg-simple_src_compile" "java-pkg-simple_src_install" "java-pkg-simple_verbose-cmd")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-utils-2
  '(("java-pkg_doexamples" "java-pkg_dojar" "depend-java-query" "java-pkg_regjar" "java-pkg_newjar" "java-pkg_addcp" "java-pkg_doso" "java-pkg_regso" "java-pkg_jarinto" "java-pkg_sointo" "java-pkg_dohtml" "java-pkg_dojavadoc" "java-pkg_dosrc" "java-pkg_dolauncher" "java-pkg_dowar" "java-pkg_recordjavadoc" "java-pkg_jar-from" "java-pkg_jarfrom" "java-pkg_getjars" "java-pkg_getjar" "java-pkg_register-dependency" "java-pkg_register-optional-dependency" "java-pkg_register-environment-variable" "java-pkg_get-bootclasspath" "java-pkg_find-normal-jars" "java-pkg_ensure-no-bundled-jars" "java-pkg_ensure-vm-version-sufficient" "java-pkg_is-vm-version-sufficient" "java-pkg_ensure-vm-version-eq" "java-pkg_is-vm-version-eq" "java-pkg_ensure-vm-version-ge" "java-pkg_is-vm-version-ge" "java-pkg_set-current-vm" "java-pkg_get-current-vm" "java-pkg_current-vm-matches" "java-pkg_get-source" "java-pkg_get-target" "java-pkg_get-javac" "java-pkg_javac-args" "java-pkg_get-jni-cflags" "java-pkg_ensure-gcj" "java-pkg_ensure-test" "java-pkg_register-ant-task" "java-pkg_ant-tasks-depend" "ejunit_" "ejunit" "ejunit4" "java-utils-2_src_prepare" "java-utils-2_pkg_preinst" "eant" "ejavac" "java-pkg_filter-compiler" "java-pkg_force-compiler" "use_doc" "java-pkg_init" "java-pkg_init-compiler_" "java-pkg_init_paths_" "java-pkg_do_write_" "java-pkg_record-jar_" "java-pkg_append_" "java-pkg_expand_dir_" "java-pkg_func-exists" "java-pkg_setup-vm" "java-pkg_needs-vm" "java-pkg_get-current-vm" "java-pkg_get-vm-vendor" "java-pkg_get-vm-version" "java-pkg_switch-vm" "java-pkg_die" "java-pkg_jar-list" "java-pkg_verify-classes" "java-pkg_ensure-dep" "java-pkg_check-phase" "java-pkg_check-versioned-jar" "java-pkg_check-jikes" "java-pkg_announce-qa-violation" "increment-qa-violations" "is-java-strict")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-virtuals-2
  '(("java-virtuals-2_src_install" "java-virtuals-2_do_write")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-vm-2
  '(("java-vm-2_pkg_setup" "java-vm-2_pkg_postinst" "java-vm_check-nsplugin" "java-vm_set-nsplugin" "java-vm-2_pkg_prerm" "java-vm-2_pkg_postrm" "java_set_default_vm_" "get_system_arch" "set_java_env" "java-vm_set-pax-markings" "java-vm_revdep-mask" "java-vm_sandbox-predict" "java_get_plugin_dir_" "install_mozilla_plugin" "java_mozilla_clean_")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-base
  '(("_calculate_src_uri" "_calculate_live_repo" "kde4-base_pkg_setup" "kde4-base_src_unpack" "kde4-base_src_prepare" "kde4-base_src_configure" "kde4-base_src_compile" "kde4-base_src_test" "kde4-base_src_install" "kde4-base_pkg_preinst" "kde4-base_pkg_postinst" "kde4-base_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-functions
  '(("buildsycoca" "comment_all_add_subdirectory" "enable_selected_linguas" "enable_selected_doc_linguas" "migrate_store_dir" "save_library_dependencies" "install_library_dependencies" "load_library_dependencies" "add_blocker" "add_kdebase_dep" "_enable_selected_linguas_dir" "get_kde_version")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-meta
  '(("kde4-meta_pkg_setup" "kde4-meta_src_unpack" "kde4-meta_src_extract" "kde4-meta_create_extractlists" "__list_needed_subdirectories" "kde4-meta_src_prepare" "_change_cmakelists_parent_dirs" "kde4-meta_change_cmakelists" "kde4-meta_src_configure" "kde4-meta_src_compile" "kde4-meta_src_test" "kde4-meta_src_install" "kde4-meta_pkg_preinst" "kde4-meta_pkg_postinst" "kde4-meta_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kernel-2
  '(("debug-print-kernel2-variables" "handle_genpatches" "detect_version" "kernel_is" "kernel_is_2_4" "kernel_is_2_6" "kernel_header_destdir" "cross_pre_c_headers" "env_setup_xmakeopts" "unpack_2_4" "unpack_2_6" "universal_unpack" "unpack_set_extraversion" "unpack_fix_install_path" "compile_headers" "compile_headers_tweak_config" "install_universal" "install_headers" "install_sources" "preinst_headers" "postinst_sources" "setup_headers" "unipatch" "getfilevar" "detect_arch" "headers___fix" "kernel-2_src_unpack" "kernel-2_src_compile" "kernel-2_src_test" "kernel-2_pkg_preinst" "kernel-2_src_install" "kernel-2_pkg_postinst" "kernel-2_pkg_setup" "kernel-2_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-latex-package
  '(("latex-package_has_tetex_3" "latex-package_src_doinstall" "latex-package_src_compile" "latex-package_src_install" "latex-package_pkg_postinst" "latex-package_pkg_postrm" "latex-package_rehash")
    font-lock-type-face))

(defvar ebuild-mode-keywords-libtool
  '(("elt_patch_dir" "ELT_try_and_apply_patch" "ELT_libtool_version" "ELT_walk_patches" "elibtoolize" "uclibctoolize" "darwintoolize" "VER_major" "VER_minor" "VER_micro" "VER_to_int")
    font-lock-type-face))

(defvar ebuild-mode-keywords-linux-info
  '(("set_arch_to_kernel" "set_arch_to_portage" "qout" "qeinfo" "qewarn" "qeerror" "getfilevar" "getfilevar_noexec" "linux_config_qa_check" "linux_config_src_exists" "linux_config_bin_exists" "linux_config_exists" "require_configured_kernel" "linux_chkconfig_present" "linux_chkconfig_module" "linux_chkconfig_builtin" "linux_chkconfig_string" "kernel_is" "get_localversion" "get_makefile_extract_function" "get_version" "get_running_version" "linux-info_get_any_version" "check_kernel_built" "check_modules_supported" "check_extra_config" "check_zlibinflate" "linux-info_pkg_setup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-linux-mod
  '(("check_vermagic" "use_m" "convert_to_m" "update_depmod" "update_modules" "move_old_moduledb" "update_moduledb" "remove_moduledb" "set_kvobj" "get-KERNEL_CC" "generate_modulesd" "find_module_params" "linux-mod_pkg_setup" "linux-mod_pkg_setup_binary" "strip_modulenames" "linux-mod_src_compile" "linux-mod_src_install" "linux-mod_pkg_preinst" "linux-mod_pkg_postinst" "linux-mod_pkg_postrm")
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
  '(("mount-boot_mount_boot_partition" "mount-boot_pkg_preinst" "mount-boot_pkg_prerm" "mount-boot_umount_boot_partition" "mount-boot_pkg_postinst" "mount-boot_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozconfig-3
  '(("mozconfig_config")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozcoreconf-2
  '(("mozconfig_annotate" "mozconfig_use_enable" "mozconfig_use_with" "mozconfig_use_extension" "moz_pkgsetup" "mozconfig_init" "makemake2" "mozconfig_final")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozextension
  '(("xpi_unpack" "xpi_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozilla-launcher
  '(("update_mozilla_launcher_symlinks" "install_mozilla_launcher_stub" "warn_mozilla_launcher_stub")
    font-lock-type-face))

(defvar ebuild-mode-keywords-multilib
  '(("has_multilib_profile" "get_libdir" "get_abi_var" "get_abi_CFLAGS" "get_abi_LDFLAGS" "get_abi_CHOST" "get_abi_CTARGET" "get_abi_FAKE_TARGETS" "get_abi_LIBDIR" "get_install_abis" "get_all_abis" "get_all_libdirs" "is_final_abi" "number_abis" "get_libname" "get_modname" "multilib_env" "multilib_toolchain_setup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-myspell
  '(("set_fields" "get_myspell_lang" "get_myspell_suffixes" "get_myspell_ooo_uri" "myspell_src_install" "myspell_pkg_postinst" "myspell_pkg_preinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql-autotools
  '(("mysql-autotools_disable_test" "mysql-autotools_configure_minimal" "mysql-autotools_configure_common" "mysql-autotools_configure_51" "pbxt_src_configure" "pbxt_src_compile" "pbxt_src_install" "mysql-autotools_src_prepare" "mysql-autotools_src_configure" "mysql-autotools_src_compile" "mysql-autotools_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql-cmake
  '(("mysql-cmake_disable_test" "configure_cmake_locale" "configure_cmake_minimal" "configure_cmake_standard" "mysql-cmake_src_prepare" "mysql-cmake_src_configure" "mysql-cmake_src_compile" "mysql-cmake_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql
  '(("pbxt_patch_available" "pbxt_available" "xtradb_patch_available" "mysql_disable_test" "mysql_init_vars" "configure_minimal" "configure_common" "configure_40_41_50" "configure_51" "pbxt_src_configure" "pbxt_src_compile" "pbxt_src_install" "mysql_pkg_setup" "mysql_src_unpack" "mysql_src_prepare" "mysql_src_configure" "mysql_src_compile" "mysql_src_install" "mysql_pkg_preinst" "mysql_pkg_postinst" "mysql_pkg_config" "mysql_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql_fx
  '(("stripdots" "mysql_check_version_range" "_mysql_test_patch_ver_pn" "mysql_mv_patches" "_mysql_mv_patches" "mysql_version_is_at_least" "mysql_lib_symlinks" "mysql_init_vars")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql-v2
  '(("pbxt_patch_available" "pbxt_available" "xtradb_patch_available" "mysql-v2_disable_test" "configure_minimal" "configure_common" "mysql-v2_pkg_setup" "mysql-v2_src_unpack" "mysql-v2_src_prepare" "mysql-v2_src_configure" "mysql-v2_src_compile" "mysql-v2_src_install" "mysql-v2_pkg_preinst" "mysql-v2_pkg_postinst" "mysql-v2_pkg_config" "mysql-v2_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mythtv-plugins
  '(("mythtv-plugins_pkg_setup" "mythtv-plugins_src_prepare" "mythtv-plugins_src_configure" "mythtv-plugins_src_compile" "mythtv-plugins_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-nsplugins
  '(("src_mv_plugins" "pkg_mv_plugins" "inst_plugin" "share_plugins_dir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-nvidia-driver
  '(("nvidia-driver-get-card" "nvidia-driver-get-mask" "nvidia-driver-check-warning")
    font-lock-type-face))

(defvar ebuild-mode-keywords-obs-service
  '(("obs-service_src_unpack" "obs-service_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-office-ext
  '(("office-ext_flush_unopkg_cache" "office-ext_get_implementation" "office-ext_add_extension" "office-ext_remove_extension" "office-ext_src_install" "office-ext_pkg_postinst" "office-ext_pkg_prerm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-openib
  '(("openib_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-pam
  '(("dopamd" "newpamd" "dopamsecurity" "newpamsecurity" "getpam_mod_dir" "pammod_hide_symbols" "dopammod" "newpammod" "pamd_mimic_system" "pamd_mimic" "cleanpamd" "pam_epam_expand")
    font-lock-type-face))

(defvar ebuild-mode-keywords-pax-utils
  '(("pax-mark" "list-paxables" "host-is-pax" "_pax_list_files")
    font-lock-type-face))

(defvar ebuild-mode-keywords-perl-app
  '(("perl-app_src_prep" "perl-app_src_configure" "perl-app_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-perl-module
  '(("perl-module_src_unpack" "perl-module_src_prepare" "perl-module_src_configure" "perl-module_src_prep" "perl-module_src_compile" "perl-module_src_test" "perl-module_src_install" "perl-module_pkg_setup" "perl-module_pkg_preinst" "perl-module_pkg_postinst" "perl-module_pkg_prerm" "perl-module_pkg_postrm" "perlinfo" "perl_set_version" "fixlocalpod" "perl_delete_localpod" "perl_fix_osx_extra" "perl_delete_module_manpages" "perl_delete_packlist" "perl_remove_temppath" "perl_link_duallife_scripts" "perl_set_eprefix")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-common-r1
  '(("php_check_cflags" "php_check_imap" "php_check_java" "php_install_java" "php_install_java_inifile" "php_check_mta" "php_check_oracle_all" "php_check_oracle_8" "php_check_pgsql" "php_get_mycnf_charset")
    font-lock-type-face))

(defvar ebuild-mode-keywords-phpconfutils
  '(("phpconfutils_sort_flags" "phpconfutils_init" "phpconfutils_usecheck" "phpconfutils_require_any" "phpconfutils_use_conflict" "phpconfutils_use_depend_all" "phpconfutils_use_depend_any" "phpconfutils_extension_disable" "phpconfutils_extension_enable" "phpconfutils_extension_without" "phpconfutils_extension_with" "phpconfutils_warn_about_external_deps" "phpconfutils_built_with_use" "phpconfutils_generate_usefile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-base-r1
  '(("php-ext-base-r1_buildinilist" "php-ext-base-r1_src_install" "php-ext-base-r1_addextension" "php-ext-base-r1_addtoinifile" "php-ext-base-r1_addtoinifiles")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-pecl-r2
  '(("php-ext-pecl-r2_src_compile" "php-ext-pecl-r2_src_install" "php-ext-pecl-r2_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-source-r2
  '(("php-ext-source-r2_src_unpack" "php-ext-source-r2_src_prepare" "php-ext-source-r2_phpize" "php-ext-source-r2_src_configure" "php-ext-source-r2_src_compile" "php-ext-source-r2_src_install" "php_get_slots" "php_init_slot_env" "php-ext-source-r2_buildinilist" "php-ext-source-r2_createinifiles" "php-ext-source-r2_addextension" "php-ext-source-r2_addtoinifile" "php-ext-source-r2_addtoinifiles")
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
  '(("treecopy" "seq" "dlopen_lib" "get_bmake" "get_mounts" "_dead_portability_user_funcs" "is-login-disabled")
    font-lock-type-face))

(defvar ebuild-mode-keywords-prefix
  '(("eprefixify")
    font-lock-type-face))

(defvar ebuild-mode-keywords-python
  '(("_python_check_python_abi_matching" "_python_package_supporting_installation_for_multiple_python_abis" "_python_parse_PYTHON_DEPEND" "_python_implementation" "_python_abi-specific_local_scope" "_python_initialize_prefix_variables" "_python_initial_sanity_checks" "_python_final_sanity_checks" "_python_set_color_variables" "_python_check_python_pkg_setup_execution" "python_pkg_setup" "python_convert_shebangs" "python_clean_installation_image" "_python_calculate_PYTHON_ABIS" "_python_prepare_flags" "_python_restore_flags" "python_execute_function" "python_copy_sources" "python_generate_wrapper_scripts" "target_executable.close" "python_merge_intermediate_installation_images" "python_wrapper_scripts_file.close" "python_set_active_version" "python_need_rebuild" "_python_get_implementation" "PYTHON" "python_get_implementation" "python_get_implementational_package" "python_get_includedir" "python_get_libdir" "python_get_sitedir" "python_get_library" "python_get_version" "python_get_implementation_and_version" "_python_test_hook" "python_execute_nosetests" "python_execute_py.test" "python_execute_trial" "python_enable_pyc" "python_disable_pyc" "_python_clean_compiled_modules" "python_mod_optimize" "python_mod_cleanup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qmail
  '(("primes" "is_prime" "dospp" "dosupervise" "qmail_set_cc" "qmail_create_groups" "qmail_create_users" "genqmail_src_unpack" "qmail_spp_src_unpack" "qmail_src_postunpack" "qmail_src_compile" "qmail_spp_src_compile" "qmail_base_install" "qmail_full_install" "qmail_config_install" "qmail_man_install" "qmail_sendmail_install" "qmail_maildir_install" "qmail_tcprules_install" "qmail_supervise_install" "qmail_spp_install" "qmail_ssl_install" "qmail_src_install" "qmail_queue_setup" "qmail_rootmail_fixup" "qmail_tcprules_fixup" "qmail_tcprules_build" "qmail_config_notice" "qmail_supervise_config_notice" "qmail_config_fast" "qmail_tcprules_config" "qmail_ssl_generate")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qt4-build
  '(("qt4-build_pkg_setup" "qt4-build_src_unpack" "qt4-build_src_prepare" "qt4-build_src_configure" "qt4-build_src_compile" "qt4-build_src_test" "fix_includes" "qt4-build_src_install" "setqtenv" "standard_configure_options" "prepare_directories" "build_directories" "install_directories" "install_qconfigs" "generate_qconfigs" "qt4-build_pkg_postrm" "qt4-build_pkg_postinst" "skip_qmake_build" "skip_project_generation" "symlink_binaries_to_buildtree" "fix_library_files" "qt_use" "qt_mkspecs_dir" "qt_assistant_cleanup" "qt_nolibx11")
font-lock-type-face))

(defvar ebuild-mode-keywords-qt4-r2
  '(("qt4-r2_src_unpack" "qt4-r2_src_prepare" "qt4-r2_src_configure" "qt4-r2_src_compile" "qt4-r2_src_install" "_find_project_file" "eqmake4")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rox-0install
  '(("0install_native_feed" "rox-0install_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rox
  '(("usemime" "expandmime" "rox_install_wrapper" "rox_install_desktop" "rox_pkg_setup" "rox_src_compile" "rox_src_install" "rox_pkg_postinst" "rox_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rpm
  '(("rpm_unpack" "srcrpm_unpack" "rpm_src_unpack" "rpm_spec_epatch")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-fakegem
  '(("ruby_fakegem_gemsdir" "ruby_fakegem_doins" "ruby_fakegem_newins" "ruby_fakegem_install_gemspec" "ruby_fakegem_gemspec_gemspec" "ruby_fakegem_metadata_gemspec" "ruby_fakegem_genspec" "ruby_fakegem_binwrapper" "all_fakegem_compile" "all_ruby_unpack" "all_ruby_compile" "each_fakegem_test" "each_fakegem_install" "each_ruby_install" "all_fakegem_install" "all_ruby_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-ng
  '(("ruby_implementation_depend" "ruby_samelib" "_ruby_atoms_samelib_generic" "ruby_implementation_command" "_ruby_atoms_samelib" "_ruby_wrap_conditions" "ruby_add_rdepend" "ruby_add_bdepend" "ruby_get_use_implementations" "ruby_get_use_targets" "ruby_implementations_depend" "_ruby_invoke_environment" "_ruby_each_implementation" "ruby-ng_pkg_setup" "ruby-ng_src_unpack" "_ruby_apply_patches" "_ruby_source_copy" "ruby-ng_src_prepare" "ruby-ng_src_configure" "ruby-ng_src_compile" "ruby-ng_src_test" "_each_ruby_check_install" "ruby-ng_src_install" "ruby_rbconfig_value" "doruby" "ruby_get_libruby" "ruby_get_hdrdir" "ruby_get_version" "ruby_get_implementation")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-ng-gnome2
  '(("each_ruby_configure" "each_ruby_compile" "each_ruby_install" "all_ruby_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-savedconfig
  '(("save_config" "restore_config" "savedconfig_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-scons-utils
  '(("escons" "scons_clean_makeopts" "use_scons")
    font-lock-type-face))

(defvar ebuild-mode-keywords-scsh
  '(("scsh_scsh_path" "set_layout" "set_path_variables" "scsh_src_unpack" "scsh_get_layout_conf" "scsh_src_compile" "scsh_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-selinux-policy-2
  '(("selinux-policy-2_src_unpack" "selinux-policy-2_src_prepare" "selinux-policy-2_src_compile" "selinux-policy-2_src_install" "selinux-policy-2_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-sgml-catalog
  '(("sgml-catalog_cat_include" "sgml-catalog_cat_doinstall" "sgml-catalog_cat_doremove" "sgml-catalog_pkg_postinst" "sgml-catalog_pkg_prerm" "sgml-catalog_pkg_postrm" "sgml-catalog_cleanup" "sgml-catalog_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ssl-cert
  '(("gen_cnf" "get_base" "gen_key" "gen_csr" "gen_crt" "gen_pem" "docert" "install_cert")
    font-lock-type-face))

(defvar ebuild-mode-keywords-stardict
  '(("stardict_src_compile" "stardict_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-subversion
  '(("subversion_fetch" "subversion_bootstrap" "subversion_src_unpack" "subversion_src_prepare" "subversion_wc_info" "subversion__svn_info" "subversion__get_repository_uri" "subversion__get_wc_path" "subversion__get_peg_revision" "subversion_pkg_preinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-sword-module
  '(("sword-module_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-systemd
  '(("_systemd_get_unitdir" "systemd_get_unitdir" "systemd_dounit" "systemd_newunit" "systemd_dotmpfilesd" "systemd_enable_service" "systemd_with_unitdir" "systemd_to_myeconfargs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-texlive-common
  '(("texlive-common_handle_config_files" "texlive-common_is_file_present_in_texmf" "texlive-common_do_symlinks" "etexlinks" "dobin_texmf_scripts" "etexmf-update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-texlive-module
  '(("texlive-module_src_unpack" "texlive-module_add_format" "texlive-module_make_language_def_lines" "texlive-module_make_language_dat_lines" "texlive-module_synonyms_to_language_lua_line" "texlive-module_make_language_lua_lines" "texlive-module_src_compile" "texlive-module_src_install" "texlive-module_pkg_postinst" "texlive-module_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-tla
  '(("tla_register_archives" "tla_check_vars" "tla_fetch" "tla_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain-binutils
  '(("is_cross" "add_src_uri" "tc-binutils_unpack" "tc-binutils_apply_patches" "toolchain-binutils_src_unpack" "toolchain-binutils_src_compile" "toolchain-binutils_src_test" "toolchain-binutils_src_install" "toolchain-binutils_pkg_postinst" "toolchain-binutils_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain
  '(("is_crosscompile" "tc_version_is_at_least" "gentoo_urls" "get_gcc_src_uri" "get_make_var" "XGCC" "hardened_gcc_works" "hardened_gcc_is_stable" "want_pie" "want_minispecs" "gcc-lang-supported" "make_gcc_hard" "create_gcc_env_entry" "setup_minispecs_gcc_build_specs" "copy_minispecs_gcc_specs" "toolchain_pkg_setup" "toolchain_pkg_preinst" "toolchain_pkg_postinst" "toolchain_pkg_prerm" "toolchain_pkg_postrm" "guess_patch_type_in_dir" "do_gcc_rename_java_bins" "toolchain_src_unpack" "gcc-abi-map" "gcc-multilib-configure" "gcc-compiler-configure" "gcc_do_configure" "toolchain_death_notice" "gcc_do_make" "gcc_do_filter_flags" "toolchain_src_compile" "toolchain_src_test" "toolchain_src_install" "gcc_slot_java" "gcc_movelibs" "gcc_quick_unpack" "do_gcc_HTB_patches" "do_gcc_PIE_patches" "should_we_gcc_config" "do_gcc_config" "gcc_version_patch" "setup_multilib_osdirnames" "disable_multilib_libjava" "fix_libtool_libdir_paths" "is_multilib" "is_cxx" "is_d" "is_f77" "is_f95" "is_fortran" "is_gcj" "is_go" "is_libffi" "is_objc" "is_objcxx" "is_ada" "is_treelang" "gcc_pkg_setup" "gcc_src_unpack" "gcc_src_compile" "gcc_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain-funcs
  '(("_tc-getPROG" "tc-getBUILD_PROG" "tc-getPROG" "tc-getAR" "tc-getAS" "tc-getCC" "tc-getCPP" "tc-getCXX" "tc-getLD" "tc-getSTRIP" "tc-getNM" "tc-getRANLIB" "tc-getOBJCOPY" "tc-getF77" "tc-getFC" "tc-getGCJ" "tc-getPKG_CONFIG" "tc-getRC" "tc-getDLLWRAP" "tc-getBUILD_AR" "tc-getBUILD_AS" "tc-getBUILD_CC" "tc-getBUILD_CPP" "tc-getBUILD_CXX" "tc-getBUILD_LD" "tc-getBUILD_STRIP" "tc-getBUILD_NM" "tc-getBUILD_RANLIB" "tc-getBUILD_OBJCOPY" "tc-getBUILD_PKG_CONFIG" "tc-export" "tc-is-cross-compiler" "tc-is-softfloat" "tc-is-hardfloat" "tc-is-static-only" "tc-env_build" "econf_build" "tc-has-openmp" "tc-has-tls" "tc-ninja_magic_to_arch" "ninj" "tc-arch-kernel" "tc-arch" "tc-endian" "_gcc_fullversion" "gcc-fullversion" "gcc-version" "gcc-major-version" "gcc-minor-version" "gcc-micro-version" "_gcc-install-dir" "_gcc-specs-exists" "_gcc-specs-directive_raw" "gcc-specs-directive" "gcc-specs-relro" "gcc-specs-now" "gcc-specs-pie" "gcc-specs-ssp" "gcc-specs-ssp-to-all" "gcc-specs-nostrict" "gen_usr_ldscript")
    font-lock-type-face))

(defvar ebuild-mode-keywords-twisted
  '(("twisted_src_test" "twisted_src_install" "_twisted_update_plugin_cache" "twisted_pkg_postinst" "twisted_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-user
  '(("_assert_pkg_ebuild_phase" "egetent" "enewuser" "enewgroup" "egethome" "egetshell")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vdr-plugin
  '(("create_plugindb_file" "delete_orphan_plugindb_file" "create_header_checksum_file" "fix_vdr_libsi_include" "vdr_patchmakefile" "vdr_add_local_patch" "vdr_has_gettext" "plugin_has_gettext" "vdr_i18n_convert_to_gettext" "vdr_i18n_disable_gettext" "vdr_i18n" "vdr-plugin_copy_source_tree" "vdr-plugin_install_source_tree" "vdr-plugin_print_enable_command" "has_vdr" "vdr-plugin_pkg_setup" "vdr-plugin_src_util" "vdr-plugin_src_unpack" "vdr-plugin_src_prepare" "vdr-plugin_src_compile" "vdr-plugin_src_install" "vdr-plugin_pkg_postinst" "vdr-plugin_pkg_postrm" "vdr-plugin_pkg_config")
    font-lock-type-face))

(defvar ebuild-mode-keywords-versionator
  '(("get_all_version_components" "get_version_components" "get_major_version" "get_version_component_range" "get_after_major_version" "replace_version_separator" "replace_all_version_separators" "delete_version_separator" "delete_all_version_separators" "get_version_component_count" "get_last_version_component_index" "version_is_at_least" "version_compare" "version_sort" "version_format_string")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-doc
  '(("update_vim_helptags")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim
  '(("apply_vim_patches" "vim_pkg_setup" "vim_src_prepare" "vim_src_unpack" "vim_src_configure" "vim_src_compile" "vim_src_install" "update_vim_symlinks" "vim_pkg_postinst" "vim_pkg_postrm" "vim_src_test")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-plugin
  '(("vim-plugin_src_install" "vim-plugin_pkg_postinst" "vim-plugin_pkg_postrm" "update_vim_afterscripts" "display_vim_plugin_help")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-spell
  '(("vim-spell_src_install" "vim-spell_pkg_postinst")
    font-lock-type-face))

(defvar ebuild-mode-keywords-virtualx
  '(("virtualmake" "Xmake" "Xemake" "Xeconf")
    font-lock-type-face))

(defvar ebuild-mode-keywords-virtuoso
  '(("virtuoso_src_prepare" "virtuoso_src_configure" "virtuoso_src_compile" "virtuoso_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vmware-bundle
  '(("vmware-bundle_extract-bundle-component" "vmware-bundle_extract-component")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vmware
  '(("vmware_create_initd" "vmware_run_questions" "vmware_determine_product" "vmware_pkg_setup" "vmware_src_unpack" "vmware_src_install" "vmware_pkg_preinst" "vmware_pkg_postinst" "vmware_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vmware-mod
  '(("vmware-mod_pkg_setup" "vmware-mod_src_unpack" "vmware-mod_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-waf-utils
  '(("waf-utils_src_configure" "waf-utils_src_compile" "waf-utils_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-webapp
  '(("webapp_read_config" "webapp_checkfileexists" "webapp_check_installedat" "webapp_strip_appdir" "webapp_strip_d" "webapp_strip_cwd" "webapp_getinstalltype" "need_httpd" "need_httpd_cgi" "need_httpd_fastcgi" "webapp_configfile" "webapp_hook_script" "webapp_postinst_txt" "webapp_postupgrade_txt" "_webapp_serverowned" "webapp_serverowned" "webapp_server_configfile" "webapp_sqlscript" "webapp_src_preinst" "webapp_pkg_setup" "webapp_src_install" "webapp_pkg_postinst" "webapp_pkg_prerm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-wxwidgets
  '(("wxwidgets_pkg_setup" "need-wxwidgets" "check_wxuse")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xemacs-elisp-common
  '(("xemacs-elisp-comp" "xemacs-elisp-site-file-install" "xemacs-elisp-site-regen")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xemacs-elisp
  '(("xemacs-elisp_src_unpack" "xemacs-elisp_src_compile")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xemacs-packages
  '(("xemacs-packages_src_unpack" "xemacs-packages_src_compile" "xemacs-packages_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xfconf
  '(("xfconf_use_debug" "xfconf_src_unpack" "xfconf_src_prepare" "xfconf_src_configure" "xfconf_src_install" "xfconf_pkg_preinst" "xfconf_pkg_postinst" "xfconf_pkg_postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-x-modular
  '(("x-modular_specs_check" "x-modular_dri_check" "x-modular_server_supports_drivers_check" "x-modular_unpack_source" "x-modular_patch_source" "x-modular_reconf_source" "x-modular_src_prepare" "x-modular_src_unpack" "x-modular_font_configure" "x-modular_debug_setup" "x-modular_src_configure" "x-modular_src_make" "x-modular_src_compile" "x-modular_src_install" "x-modular_pkg_preinst" "x-modular_pkg_postinst" "x-modular_pkg_postrm" "setup_fonts" "remove_font_metadata" "install_driver_hwdata" "discover_font_dirs" "create_fonts_scale" "create_fonts_dir" "create_font_cache")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xorg-2
  '(("xorg-2_pkg_setup" "xorg-2_src_unpack" "xorg-2_patch_source" "xorg-2_reconf_source" "xorg-2_src_prepare" "xorg-2_font_configure" "xorg-2_flags_setup" "xorg-2_src_configure" "xorg-2_src_compile" "xorg-2_src_install" "xorg-2_pkg_postinst" "xorg-2_pkg_postrm" "remove_font_metadata" "create_fonts_scale" "create_fonts_dir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-zproduct
  '(("zproduct_src_install" "docs_move" "zproduct_pkg_postinst" "zproduct_pkg_prerm" "zproduct_pkg_config")
    font-lock-type-face))

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; no-byte-compile: t
;; End:

;;; ebuild-mode-keywords.el ends here
