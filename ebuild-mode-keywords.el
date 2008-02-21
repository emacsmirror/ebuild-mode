;;; ebuild-mode-keywords.el

;; Copyright 2006-2008 Gentoo Foundation

;; Author: Matthew Kennedy <mkennedy@gentoo.org>
;;	Diego Pettenò <flameeyes@gentoo.org>
;;	Christian Faulhammer <opfer@gentoo.org>
;;	Ulrich Müller <ulm@gentoo.org>
;; Maintainer: <emacs@gentoo.org>
;; Keywords: languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The commands have been grouped into lists of source (eclass).
;; We map each set of keywords to the basic faces: font-lock-*-face.

;;; Code:

;; eclasses which use is deprecated: gcc

(defvar ebuild-mode-keywords-0
  '("best_version" "check_KV" "die" "diropts" "dobin" "docinto" "doconfd"
    "dodir" "dodoc" "doenvd" "doexe" "dohard" "dohtml" "doinfo" "doinitd"
    "doins" "dojar" "dolib" "dolib.a" "dolib.so" "doman" "domo" "dopython"
    "dosbin" "dosed" "dosym" "ebegin" "econf" "eend" "eerror" "einfo" "einfon"
    "einstall" "elog" "emake" "ewarn" "exeinto" "exeopts" "fowners" "fperms"
    "has" "has_version" "hasq" "hasv" "insinto" "insopts" "into" "keepdir"
    "libopts" "newbin" "newconfd" "newdoc" "newenvd" "newexe" "newinitd"
    "newins" "newlib.a" "newlib.so" "newman" "newsbin" "prepall" "prepalldocs"
    "prepallinfo" "prepallman" "prepallstrip" "unpack" "use" "use_enable"
    "use_with" "useq" "usev"))

(defvar ebuild-mode-keywords-functions
  '("pkg_nofetch" "pkg_setup" "src_unpack" "src_compile" "src_test"
    "src_install" "pkg_preinst" "pkg_postinst" "pkg_prerm" "pkg_postrm"
    "pkg_config"))

(defvar ebuild-mode-keywords-warn
  ;; warn about "which" usage
  ;; see http://permalink.gmane.org/gmane.linux.gentoo.devel/46770
  '("which"))

(defvar ebuild-mode-keywords-gnustep-funcs
  '("egnustep_prefix" "egnustep_install_domain" "egnustep_env"
    "egnustep_system_root" "egnustep_local_root" "egnustep_network_root"
    "egnustep_user_root" "egnustep_user_root_suffix" "egnustep_make"
    "egnustep_package_config" "egnustep_package_config_info"
    "egnustep_install" "egnustep_doc" "objc_available"
    "objc_not_available_info" "ffi_available" "ffi_not_available_info"))

(defvar ebuild-mode-keywords-freebsd
  '("doperiodic" "freebsd_get_bmake" "freebsd_do_patches"
    "freebsd_rename_libraries"))

(defvar ebuild-mode-keywords-cron
  '("docrondir" "docron" "docrontab"))

(defvar ebuild-mode-keywords-toolchain-binutils
  '("is_cross" "add_src_uri" "tc-binutils_unpack"
    "tc-binutils_apply_patches"))

(defvar ebuild-mode-keywords-toolchain-funcs
  '("tc-getPROG" "tc-getAR" "tc-getAS" "tc-getCC" "tc-getCPP" "tc-getCXX"
    "tc-getLD" "tc-getSTRIP" "tc-getNM" "tc-getRANLIB" "tc-getF77" "tc-getF90"
    "tc-getFORTRAN" "tc-getGCJ" "tc-getBUILD_CC" "tc-export"
    "tc-is-cross-compiler" "tc-ninja_magic_to_arch" "tc-arch-kernel" "tc-arch"
    "tc-endian" "gcc-fullversion" "gcc-version" "gcc-major-version"
    "gcc-minor-version" "gcc-micro-version"))

(defvar ebuild-mode-keywords-toolchain
  '("is_crosscompile" "tc_version_is_at_least" "gcc_get_s_dir" "gentoo_urls"
    "get_gcc_src_uri" "get_make_var" "XGCC" "hardened_gcc_works"
    "hardened_gcc_is_stable" "hardened_gcc_check_unsupported" "has_libssp"
    "want_libssp" "want_boundschecking" "want_pie" "want_ssp"
    "want_split_specs" "glibc_have_pie" "libc_has_ssp" "gcc-lang-supported"
    "make_gcc_hard" "create_vanilla_specs_file" "create_hardened_specs_file"
    "create_hardenednossp_specs_file" "create_hardenednopie_specs_file"
    "create_hardenednopiessp_specs_file" "split_out_specs_files"
    "create_gcc_env_entry" "add_profile_eselect_conf" "create_eselect_conf"
    "guess_patch_type_in_dir" "do_gcc_rename_java_bins"
    "gcc-library-configure" "gcc-compiler-configure" "gcc_do_configure"
    "gcc_do_make" "add_version_to_shared" "gcc_do_filter_flags" "gcc_movelibs"
    "gcc_quick_unpack" "exclude_gcc_patches" "do_gcc_stub"
    "do_gcc_HTB_patches" "do_gcc_SSP_patches" "update_gcc_for_libc_ssp"
    "update_gcc_for_libssp" "do_gcc_PIE_patches" "should_we_gcc_config"
    "do_gcc_config" "should_we_eselect_compiler" "do_eselect_compiler"
    "gcc_version_patch" "disgusting_gcc_multilib_HACK"
    "disable_multilib_libjava" "fix_libtool_libdir_paths" "is_multilib"
    "is_cxx" "is_d" "is_f77" "is_f95" "is_fortran" "is_gcj" "is_objc"
    "is_objcxx" "is_ada"))

(defvar ebuild-mode-keywords-libtool
  '("elibtoolize" "uclibctoolize" "darwintoolize"))

(defvar ebuild-mode-keywords-fixheadtails
  '("ht_fix_file" "ht_fix_all"))

(defvar ebuild-mode-keywords-webapp
  '("webapp_checkfileexists" "webapp_import_config" "webapp_strip_appdir"
    "webapp_strip_d" "webapp_strip_cwd" "webapp_configfile"
    "webapp_hook_script" "webapp_postinst_txt" "webapp_postupgrade_txt"
    "webapp_runbycgibin" "webapp_serverowned" "webapp_server_configfile"
    "webapp_sqlscript" "webapp_src_install" "webapp_pkg_postinst"
    "webapp_pkg_setup" "webapp_getinstalltype" "webapp_src_preinst"
    "webapp_pkg_prerm"))

(defvar ebuild-mode-keywords-darcs
  '("darcs_src_unpack"))

(defvar ebuild-mode-keywords-php-common-r1
  '("php_check_cflags" "php_check_imap" "php_check_java" "php_install_java"
    "php_install_java_inifile" "php_check_mta" "php_check_oracle_all"
    "php_check_oracle_8" "php_check_pgsql" "php_get_mycnf_charset"))

(defvar ebuild-mode-keywords-php-ext-base-r1
  '("php-ext-base-r1_buildinilist" "php-ext-base-r1_src_install"
    "php-ext-base-r1_addextension" "php-ext-base-r1_addtoinifile"
    "php-ext-base-r1_addtoinifiles"))

(defvar ebuild-mode-keywords-php-ext
  '("php-ext_buildinilist" "php-ext_extension_is_present"
    "php-ext_addextensiontoinifile" "php-ext_addextension"
    "php-ext_setting_is_present" "php-ext_addtoinifile"
    "php-ext_addtoinifiles"))

(defvar ebuild-mode-keywords-php-pear-r1
  '("fix_PEAR_PV"))

(defvar ebuild-mode-keywords-php-sapi
  '("php-sapi_is_providerbuild" "php-sapi_check_java_config"
    "php-sapi_securityupgrade" "php-sapi_warning_mssql_freetds"))

(defvar ebuild-mode-keywords-php4_4-sapi
  '("php4_4-sapi_check_use_flags" "php4_4-sapi_set_php_ini_dir"
    "php4_4-sapi_install_ini"))

(defvar ebuild-mode-keywords-php5-sapi-r3
  '("php5-sapi-r3_check_awkward_uses"))

(defvar ebuild-mode-keywords-mozilla-launcher
  '("update_mozilla_launcher_symlinks" "install_mozilla_launcher_stub"
    "warn_mozilla_launcher_stub"))

(defvar ebuild-mode-keywords-mozconfig
  '("mozconfig_init" "makemake" "mozconfig_annotate" "mozconfig_use_enable"
    "mozconfig_use_with" "mozconfig_use_extension" "mozconfig_final"))

(defvar ebuild-mode-keywords-mozconfig-2
  '("mozconfig_config"))

(defvar ebuild-mode-keywords-mozcoreconf
  '("mozconfig_init" "makemake" "makemake2" "mozconfig_annotate"
    "mozconfig_use_enable" "mozconfig_use_with" "mozconfig_use_extension"
    "mozconfig_final"))

(defvar ebuild-mode-keywords-mozextensions
  '("xpi_unpack" "xpi_install"))

(defvar ebuild-mode-keywords-db
  '("db_fix_so" "db_src_install_usrbinslot" "db_src_install_headerslota"
    "db_src_install_usrlibcleanup"))

(defvar ebuild-mode-keywords-db-use
  '("db_ver_to_slot" "db_findver" "db_includedir" "db_libname"))

(defvar ebuild-mode-keywords-db4-fix
  '("dodb4-fix"))

(defvar ebuild-mode-keywords-64-bit
  '("64-bit"))

(defvar ebuild-mode-keywords-xfce44
  '("xfce44_beta" "xfce44" "xfce44_gzipped" "xfce44_zipped" "xfce44_plugin"
    "xfce44_panel_plugin" "xfce44_thunar_plugin" "xfce44_goodies"
    "xfce44_goodies_panel_plugin" "xfce44_goodies_thunar_plugin"
    "xfce44_core_package" "xfce44_extra_package" "xfce44_single_make"
    "xfce44_want_einstall"))

(defvar ebuild-mode-keywords-x11
  '("vcards" "filter-patch" "check_version_h" "get_KV_info" "is_kernel"
    "strip_bins" "arch" "archq" "use_build"))

(defvar ebuild-mode-keywords-confutils
  '("confutils_init" "confutils_require_any" "confutils_use_conflict"
    "confutils_use_depend_all" "confutils_use_depend_any"
    "enable_extension_disable" "enable_extension_enable"
    "enable_extension_enableonly" "enable_extension_without"
    "enable_extension_with" "enable_extension_withonly"
    "confutils_warn_about_missing_deps" "enable_extension_enable_built_with"
    "enable_extension_with_built_with"))

(defvar ebuild-mode-keywords-linux-info
  '("qout" "qeinfo" "qeerror" "getfilevar" "linux_chkconfig_present"
    "linux_chkconfig_module" "linux_chkconfig_builtin"
    "linux_chkconfig_string" "kernel_is" "get_localversion" "get_version"
    "get_running_version" "check_kernel_built" "check_modules_supported"
    "check_extra_config" "check_zlibinflate"))

(defvar ebuild-mode-keywords-linux-mod
  '("check_vermagic" "use_m" "convert_to_m" "update_depmod" "update_modules"
    "move_old_moduledb" "update_moduledb" "remove_moduledb" "set_kvobj"
    "get-KERNEL_CC" "generate_modulesd" "find_module_params"
    "strip_modulenames"))

(defvar ebuild-mode-keywords-nsplugin
  '("src_mv_plugins" "pkg_mv_plugins" "inst_plugin"))

(defvar ebuild-mode-keywords-latex-package
  '("latex-package_has_tetex_3" "latex-package_src_doinstall"
    "latex-package_rehash" "latex-package_pkg_postinst"
    "latex-package_pkg_postrm" "latex-package_src_compile"
    "latex-package_src_install"))

(defvar ebuild-mode-keywords-autotools
  '("eautoreconf" "eaclocal" "_elibtoolize" "eautoconf" "eautoheader"
    "eautomake"))

(defvar ebuild-mode-keywords-gtk-sharp-component
  '("gtk-sharp-component_fix_makefiles"))

(defvar ebuild-mode-keywords-qt3
  '("qt_min_version" "qt_min_version_list"))

(defvar ebuild-mode-keywords-qt4
  '("qt4_min_version" "qt4_min_version_list"))

(defvar ebuild-mode-keywords-kde-functions
  '("get-parent-package" "get-child-packages" "is-parent-package"
    "need-automake" "need-autoconf" "deprange" "deprange-list"
    "deprange-iterate-numbers" "deprange-iterate-suffixes" "deprange-dual"
    "need-kde" "set-kdedir" "need-qt" "set-qtdir" "qtver-from-kdever"
    "min-kde-ver" "kde_sandbox_patch" "kde_remove_flag" "buildsycoca"
    "postprocess_desktop_entries"))

(defvar ebuild-mode-keywords-kde-meta
  '("create_fullpaths" "change_makefiles" "set_common_variables"))

(defvar ebuild-mode-keywords-kde
  '("slot_rebuild"))

(defvar ebuild-mode-keywords-gst-plugins
  '("gst-plugins_find_plugin_dir" "gst-plugins_update_registry"
    "gst-plugins_remove_unversioned_binaries"))

(defvar ebuild-mode-keywords-gst-plugins10
  '("gst-plugins10_find_plugin_dir"
    "gst-plugins10_remove_unversioned_binaries"))

(defvar ebuild-mode-keywords-libtool
  '("ELT_find_ltmain_sh" "ELT_try_and_apply_patch" "ELT_libtool_version"
    "ELT_walk_patches" "elibtoolize" "VER_major" "VER_minor" "VER_micro"
    "VER_to_int"))

(defvar ebuild-mode-keywords-multilib
  '("has_multilib_profile" "get_libdir" "get_multilibdir"
    "get_libdir_override" "get_abi_var" "get_install_abis" "get_all_abis"
    "get_all_libdirs" "is_final_abi" "number_abis" "get_ml_incdir"
    "prep_ml_includes" "create_ml_includes" "get_libname" "multilib_env"
    "multilib_toolchain_setup" "get_abi_CFLAGS" "get_abi_LDFLAGS"
    "get_abi_CHOST" "get_abi_FAKE_TARGETS" "get_abi_CDEFINE" "get_abi_LIBDIR"
    "create_ml_includes-absolute" "create_ml_includes-tidy_path"
    "create_ml_includes-listdirs" "create_ml_includes-makedestdirs"
    "create_ml_includes-allfiles" "create_ml_includes-sym_for_dir"))

(defvar ebuild-mode-keywords-java-ant-2
  '("java-ant_bsfix_files" "java-ant_bsfix_one" "java-ant_rewrite-classpath"
    "java-ant_ignore-system-classes" "java-ant_xml-rewrite"))

(defvar ebuild-mode-keywords-java-utils-2
  '("java-pkg_doexamples" "java-pkg_dojar" "java-pkg_regjar" "java-pkg_newjar"
    "java-pkg_addcp" "java-pkg_doso" "java-pkg_regso" "java-pkg_jarinto"
    "java-pkg_sointo" "java-pkg_dohtml" "java-pkg_dojavadoc" "java-pkg_dosrc"
    "java-pkg_dolauncher" "java-pkg_dowar" "java-pkg_jar-from"
    "java-pkg_jarfrom" "java-pkg_getjars" "java-pkg_getjar"
    "java-pkg_register-dependency" "java-pkg_register-optional-dependency"
    "java-pkg_register-environment-variable" "java-pkg_find-normal-jars"
    "java-pkg_ensure-no-bundled-jars" "java-pkg_get-source"
    "java-pkg_set-current-vm" "java-pkg_get-current-vm"
    "java-pkg_current-vm-matches" "java-pkg_get-target" "java-pkg_get-javac"
    "java-pkg_javac-args" "java-pkg_get-jni-cflags" "java-pkg_ensure-gcj"
    "java-pkg_ensure-test" "java-pkg_register-ant-task" "ejunit" "eant"
    "ejavac" "java-pkg_filter-compiler" "java-pkg_force-compiler" "use_doc"))

(defvar ebuild-mode-keywords-bash-completion
  '("dobashcompletion" "bash-completion_pkg_postinst"))

(defvar ebuild-mode-keywords-fdo-mime
  '("fdo-mime_desktop_database_update" "fdo-mime_mime_database_update"))

(defvar ebuild-mode-keywords-gnome2-utils
  '("gnome2_gconf_install" "gconf_uninstall" "icon_cache_update"
    "gnome2_omf_fix" "gnome2_scrollkeeper_update"))

(defvar ebuild-mode-keywords-gnome2
  '("gnome2_src_configure" "gnome2_src_compile" "gnome2_src_install"
    "gnome2_gconf_uninstall" "gnome2_pkg_postinst" "gnome2_pkg_postrm"))

(defvar ebuild-mode-keywords-alternatives
  '("alternatives_pkg_postinst" "alternatives_pkg_postrm"
    "alternatives_makesym" "alternatives_auto_makesym"))

(defvar ebuild-mode-keywords-eutils
  '("epause" "ebeep" "epatch" "emktemp" "enewuser" "enewgroup" "edos2unix"
    "make_desktop_entry" "validate_desktop_entries" "make_session_desktop"
    "domenu" "newmenu" "doicon" "newicon" "check_license" "cdrom_get_cds"
    "cdrom_load_next_cd" "strip-linguas" "set_arch_to_kernel"
    "set_arch_to_portage" "preserve_old_lib" "preserve_old_lib_notify"
    "built_with_use" "epunt_cxx" "make_wrapper" "gen_usr_ldscript" "draw_line"
    "have_NPTL" "get_number_of_jobs" "egetent" "unpack_pdv" "unpack_makeself"
    "cdrom_load_next" "cdrom_locate_file_on_cd" "find_unpackable_file"))

(defvar ebuild-mode-keywords-pam
  '("dopamd" "newpamd" "dopamsecurity" "newpamsecurity" "getpam_mod_dir"
    "dopammod" "newpammod" "pamd_mimic" "clean_pamd"))

(defvar ebuild-mode-keywords-mysql
  '("bitkeeper_fetch" "mysql_disable_test" "mysql_init_vars"
    "configure_minimal" "configure_common" "configure_40_41_50"
    "configure_51"))

(defvar ebuild-mode-keywords-mysql_fx
  '("stripdots" "mysql_check_version_range" "mysql_mv_patches"
    "mysql_version_is_at_least" "mysql_lib_symlinks"))

(defvar ebuild-mode-keywords-cvs
  '("cvs_src_unpack" "cvs-fetch"))

(defvar ebuild-mode-keywords-subversion
  '("subversion_src_unpack" "subversion_svn_fetch" "subversion_bootstrap"))

(defvar ebuild-mode-keywords-git
  '("git_src_unpack"))

(defvar ebuild-mode-keywords-mercurial
  '("mercurial_src_unpack"))

(defvar ebuild-mode-keywords-rpm
  '("rpm_src_unpack"))

(defvar ebuild-mode-keywords-vim
  '("apply_vim_patches" "update_vim_symlinks"))

(defvar ebuild-mode-keywords-vim-doc
  '("update_vim_helptags"))

(defvar ebuild-mode-keywords-vim-plugin
  '("vim-plugin_src_install" "vim-plugin_pkg_postinst" "vim-plugin_pkg_postrm"
    "update_vim_afterscripts" "display_vim_plugin_help"))

(defvar ebuild-mode-keywords-sandbox
  '("adddeny" "addpredict" "addread" "addwrite"))

(defvar ebuild-mode-keywords-eclass
  '("inherit"))

(defvar ebuild-mode-keywords-flag-o-matic
  '("append-flags" "append-ldflags" "filter-flags" "filter-ldflags"
    "filter-mfpmath" "get-flag" "is-flag" "replace-cpu-flags" "replace-flags"
    "strip-flags" "strip-unsupported-flags" "setup-allowed-flags"
    "filter-lfs-flags" "append-lfs-flags" "test_flag" "test_version_info"
    "has_hardened" "has_pic" "has_pie" "has_ssp_all" "has_ssp" "has_m64"
    "has_m32" "replace-sparc64-flags" "fstack-flags" "gcc2-flags"))

(defvar ebuild-mode-keywords-python
  '("NEED_PYTHON" "DOCS" "python_version" "python_tkinter_exists"
    "python_mod_exists" "python_mod_compile" "python_mod_optimize"
    "python_mod_cleanup" "python_makesym" "python_disable_pyc"
    "python_enable_pyc"))

(defvar ebuild-mode-keywords-common-lisp-common-3
  '("do-debian-credits" "standard-impl-postinst" "standard-impl-postrm"))

(defvar ebuild-mode-keywords-common-lisp-common-2
  '("do-debian-credits" "standard-impl-postinst" "standard-impl-postrm"))

(defvar ebuild-mode-keywords-common-lisp-common
  '("do-debian-credits" "standard-impl-postinst" "standard-impl-postrm"
    "register-common-lisp-implementation"
    "unregister-common-lisp-implementation"
    "reregister-all-common-lisp-implementations"))

(defvar ebuild-mode-keywords-common-lisp
  ;; common-lisp-system-symlink is obsolete
  '("common-lisp-symlink-asdf" "common-lisp-system-symlink"
    "common-lisp-install"))

(defvar ebuild-mode-keywords-ruby
  '("ruby_econf" "ruby_emake" "doruby" "ruby_einstall" "erubydoc" "erubyconf"
    "erubymake" "erubyinstall" "RUBY_OPTIONAL"))

(defvar ebuild-mode-keywords-check-kernel
  '("is_2_4_kernel" "is_2_5_kernel" "is_2_6_kernel"
    "kernel_supports_modules"))

(defvar ebuild-mode-keywords-elisp-common
  '("elisp-comp" "elisp-compile" "elisp-install" "elisp-site-file-install"
    "elisp-site-regen" "elisp-emacs-version" "elisp-make-autoload-file"))

(defvar ebuild-mode-keywords-elisp
  '("NEED_EMACS" "DOCS"))

(defvar ebuild-mode-keywords-games-etmod
  '("games-etmod_make_etded_exec" "games-etmod_make_enemy-territory_exec"
    "games-etmod_make_init.d" "games-etmod_make_conf.d"))

(defvar ebuild-mode-keywords-games-mods
  '("default_client" "games-mods_make_ded_exec" "games-mods_make_init.d"
    "games-mods_make_conf.d"))

(defvar ebuild-mode-keywords-games-q3mod
  '("games-q3mod_make_q3ded_exec" "games-q3mod_make_quake3_exec"
    "games-q3mod_make_init.d" "games-q3mod_make_conf.d"))

(defvar ebuild-mode-keywords-games-ut2k4mod
  '("check_dvd"))

(defvar ebuild-mode-keywords-games
  '("games_get_libdir" "egamesconf" "egamesinstall" "gameswrapper"
    "dogamesbin" "dogamessbin" "dogameslib" "dogameslib.a" "dogameslib.so"
    "newgamesbin" "newgamessbin" "games_make_wrapper" "gamesowners"
    "gamesperms" "prepgamesdirs" "gamesenv" "games_ut_unpack"
    "games_umod_unpack" "games_link_mods" "games_ut_unpack"))

(defvar ebuild-mode-keywords-perl-app
  '("perl-app_src_prep"))

(defvar ebuild-mode-keywords-perl-module
  '("perlinfo" "fixlocalpod" "updatepod"))

(defvar ebuild-mode-keywords-distutils
  '("distutils_python_version" "distutils_python_tkinter"))

(defvar ebuild-mode-keywords-depend-apache
  '("need_apache" "need_apache1" "need_apache2"))

(defvar ebuild-mode-keywords-apache-module
  '("apache-module_pkg_setup" "apache-module_src_compile"
    "apache-module_src_install" "apache-module_pkg_postinst" "apache_cd_dir"
    "apache_mod_file" "apache_doc_magic"))

(defvar ebuild-mode-keywords-kernel-2
  '("debug-print-kernel2-variables" "handle_genpatches" "detect_version"
    "kernel_is" "kernel_is_2_4" "kernel_is_2_6" "kernel_header_destdir"
    "cross_pre_c_headers" "env_setup_xmakeopts" "unpack_2_4" "unpack_2_6"
    "universal_unpack" "unpack_set_extraversion" "unpack_fix_install_path"
    "compile_headers" "compile_headers_tweak_config" "install_universal"
    "install_headers" "install_sources" "preinst_headers" "postinst_sources"
    "postinst_headers" "setup_headers" "unipatch" "getfilevar" "detect_arch"
    "generate_sparc_asm" "headers___fix"))

(defvar ebuild-mode-keywords-kernel-mod
  '("kernel-mod_getmakefilevar" "kernel-mod_getversion"
    "kernel-mod_configoption_present" "kernel-mod_configoption_module"
    "kernel-mod_configoption_builtin" "kernel-mod_modules_supported"
    "kernel-mod_check_modules_supported"
    "kernel-mod_checkzlibinflate_configured" "kernel-mod_is_2_4_kernel"
    "kernel-mod_is_2_5_kernel" "kernel-mod_is_2_6_kernel"))

(defvar ebuild-mode-keywords-versionator
  '("get_all_version_components" "get_version_components" "get_major_version"
    "get_version_component_range" "get_after_major_version"
    "replace_version_separator" "replace_all_version_separators"
    "delete_version_separator" "delete_all_version_separators"
    "get_version_component_count" "get_last_version_component_index"
    "version_is_at_least" "version_compare" "version_sort"))

(font-lock-add-keywords
 'ebuild-mode
 (mapcar
  '(lambda (x) (apply 'ebuild-mode-make-keywords-list x))
  (list	(list ebuild-mode-keywords-sandbox font-lock-warning-face)
   	(list ebuild-mode-keywords-warn font-lock-warning-face)
   	(list ebuild-mode-keywords-0 font-lock-type-face)
	(list ebuild-mode-keywords-functions font-lock-type-face)
   	(list ebuild-mode-keywords-perl-app font-lock-type-face)
   	(list ebuild-mode-keywords-perl-module font-lock-type-face)
   	(list ebuild-mode-keywords-distutils font-lock-type-face)
   	(list ebuild-mode-keywords-games-etmod font-lock-type-face)
   	(list ebuild-mode-keywords-games-mods font-lock-type-face)
   	(list ebuild-mode-keywords-games-q3mod font-lock-type-face)
   	(list ebuild-mode-keywords-games-ut2k4mod font-lock-type-face)
   	(list ebuild-mode-keywords-games font-lock-type-face)
   	(list ebuild-mode-keywords-depend-apache font-lock-type-face)
   	(list ebuild-mode-keywords-apache-module font-lock-type-face)
   	(list ebuild-mode-keywords-kernel-2 font-lock-type-face)
   	(list ebuild-mode-keywords-kernel-mod font-lock-type-face)
   	(list ebuild-mode-keywords-check-kernel font-lock-type-face)
   	(list ebuild-mode-keywords-mysql font-lock-type-face)
   	(list ebuild-mode-keywords-mysql_fx font-lock-type-face)
	(list ebuild-mode-keywords-gst-plugins10 font-lock-type-face)
	(list ebuild-mode-keywords-gst-plugins font-lock-type-face)
	(list ebuild-mode-keywords-gnustep-funcs font-lock-type-face)
	(list ebuild-mode-keywords-php-ext-base-r1 font-lock-type-face)
	(list ebuild-mode-keywords-php-common-r1 font-lock-type-face)
	(list ebuild-mode-keywords-php-ext font-lock-type-face)
	(list ebuild-mode-keywords-php-pear-r1 font-lock-type-face)
	(list ebuild-mode-keywords-php-sapi font-lock-type-face)
	(list ebuild-mode-keywords-php4_4-sapi font-lock-type-face)
	(list ebuild-mode-keywords-php5-sapi-r3 font-lock-type-face)
	(list ebuild-mode-keywords-toolchain font-lock-type-face)
	(list ebuild-mode-keywords-toolchain-binutils font-lock-type-face)
	(list ebuild-mode-keywords-toolchain-funcs font-lock-type-face)
	(list ebuild-mode-keywords-eutils font-lock-type-face)
	(list ebuild-mode-keywords-bash-completion font-lock-type-face)
	(list ebuild-mode-keywords-gnome2-utils font-lock-type-face)
	(list ebuild-mode-keywords-gnome2 font-lock-type-face)
	(list ebuild-mode-keywords-fdo-mime font-lock-type-face)
	(list ebuild-mode-keywords-java-ant-2 font-lock-type-face)
	(list ebuild-mode-keywords-java-utils-2 font-lock-type-face)
	(list ebuild-mode-keywords-alternatives font-lock-type-face)
	(list ebuild-mode-keywords-pam font-lock-type-face)
	(list ebuild-mode-keywords-mozilla-launcher font-lock-type-face)
	(list ebuild-mode-keywords-mozconfig font-lock-type-face)
	(list ebuild-mode-keywords-mozconfig-2 font-lock-type-face)
	(list ebuild-mode-keywords-mozcoreconf font-lock-type-face)
	(list ebuild-mode-keywords-mozextensions font-lock-type-face)
	(list ebuild-mode-keywords-db font-lock-type-face)
	(list ebuild-mode-keywords-db4-fix font-lock-type-face)
	(list ebuild-mode-keywords-db-use font-lock-type-face)
	(list ebuild-mode-keywords-xfce44 font-lock-type-face)
	(list ebuild-mode-keywords-autotools font-lock-type-face)
	(list ebuild-mode-keywords-cvs font-lock-type-face)
	(list ebuild-mode-keywords-subversion font-lock-type-face)
	(list ebuild-mode-keywords-git font-lock-type-face)
	(list ebuild-mode-keywords-rpm font-lock-type-face)
	(list ebuild-mode-keywords-mercurial font-lock-type-face)
	(list ebuild-mode-keywords-ruby font-lock-type-face)
	(list ebuild-mode-keywords-qt3 font-lock-type-face)
	(list ebuild-mode-keywords-qt4 font-lock-type-face)
	(list ebuild-mode-keywords-freebsd font-lock-type-face)
	(list ebuild-mode-keywords-cron font-lock-type-face)
	(list ebuild-mode-keywords-libtool font-lock-type-face)
	(list ebuild-mode-keywords-fixheadtails font-lock-type-face)
	(list ebuild-mode-keywords-webapp font-lock-type-face)
	(list ebuild-mode-keywords-64-bit font-lock-type-face)
	(list ebuild-mode-keywords-darcs font-lock-type-face)
	(list ebuild-mode-keywords-nsplugin font-lock-type-face)
	(list ebuild-mode-keywords-latex-package font-lock-type-face)
	(list ebuild-mode-keywords-gtk-sharp-component font-lock-type-face)
	(list ebuild-mode-keywords-libtool font-lock-type-face)
	(list ebuild-mode-keywords-kde font-lock-type-face)
	(list ebuild-mode-keywords-kde-meta font-lock-type-face)
	(list ebuild-mode-keywords-kde-functions font-lock-type-face)
	(list ebuild-mode-keywords-x11 font-lock-type-face)
	(list ebuild-mode-keywords-vim font-lock-type-face)
	(list ebuild-mode-keywords-vim-doc font-lock-type-face)
	(list ebuild-mode-keywords-vim-plugin font-lock-type-face)
	(list ebuild-mode-keywords-confutils font-lock-type-face)
	(list ebuild-mode-keywords-linux-info font-lock-type-face)
	(list ebuild-mode-keywords-linux-mod font-lock-type-face)
	(list ebuild-mode-keywords-common-lisp-common-3 font-lock-type-face)
	(list ebuild-mode-keywords-common-lisp-common-2 font-lock-type-face)
	(list ebuild-mode-keywords-common-lisp-common font-lock-type-face)
	(list ebuild-mode-keywords-common-lisp font-lock-type-face)
	(list ebuild-mode-keywords-multilib font-lock-type-face)
	(list ebuild-mode-keywords-eclass font-lock-type-face)
	(list ebuild-mode-keywords-versionator font-lock-type-face)
	(list ebuild-mode-keywords-flag-o-matic font-lock-type-face)
	(list ebuild-mode-keywords-elisp font-lock-type-face)
	(list ebuild-mode-keywords-elisp-common font-lock-type-face))))

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; End:

;;; ebuild-mode-keywords.el ends here
