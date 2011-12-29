;;; ebuild-mode-keywords.el

;; Copyright 2006-2011 Gentoo Foundation

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
  '(("which" "EAPI" "bindnow-flags")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-deprecated
  ;; deprecated eclass functions
  '(("elisp-comp" "prepalldocs" "dosed" "dohard" "python_mod_compile" "dobashcompletion" "bash-completion_pkg_postinst")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-base
  '(("base_src_work" "base_src_install_docs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-check_reqs
  '(("check-reqs_pkg_setup" "check-reqs_pkg_pretend" "check-reqs_prepare"
     "check-reqs_run" "check-reqs_get_mebibytes" "check-reqs_get_number"
     "check-reqs_get_unit" "check-reqs_output" "check-reqs_memory"
     "check-reqs_disk" "check-reqs_start_phase" "check-reqs_unsatisfied")
    font-lock-type-face))

(defvar ebuild-mode-keywords-office-ext
  '(("office-ext_flush_unopkg_cache" "office-ext_get_implementation" "office-ext_add_extension"
     "office-ext_remove_extension")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ghc-packages
  '(("ghc-getghc" "ghc-getghcpkg" "ghc-getghcpkgbin" "ghc-version" "ghc-cabal"
     "ghc-bestcabalversion" "ghc-sanecabal" "ghc-saneghc" "ghc-supports-shared-libraries"
     "ghc-extractportageversion" "ghc-libdir" "ghc-confdir" "ghc-localpkgconf"
     "ghc-makeghcilib" "ghc-fixlibpath" "ghc-setup-pkg" "ghc-install-pkg"
     "ghc-register-pkg" "ghc-reregister" "ghc-unregister-pkg" "ghc-reverse"
     "ghc-elem" "ghc-listpkg")
    font-lock-type-face))

(defvar ebuild-mode-keywords-systemd
  '(("systemd_get_unitdir" "systemd_dounit" "systemd_enable_service" "systemd_with_unitdir"
     "systemd_to_myeconfargs" "systemd_dotmpfilesd" "systemd_newunit")
    font-lock-type-face))

(defvar ebuild-mode-keywords-go-mono
  '(("go-mono_src_prepare" "go-mono_src_configure" "go-mono_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mono
  '(("egacinstall" "mono_multilib_comply")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cmake-utils
  '(("cmake-utils_use_with" "cmake-utils_use_enable" "cmake-utils_use_disable"
     "cmake-utils_use_no" "cmake-utils_use_want" "cmake-utils_use_build"
     "cmake-utils_use_has" "cmake-utils_has" "cmake-utils_use"
     "cmake-utils_src_configure" "cmake-utils_src_compile"
     "cmake-utils_src_make" "cmake-utils_src_install" "cmake-utils_src_test")
  font-lock-type-face))

(defvar ebuild-mode-keywords-freebsd
  '(("doperiodic" "freebsd_get_bmake" "freebsd_do_patches"
     "freebsd_rename_libraries")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cron
  '(("docrondir" "docron" "docrontab")
    font-lock-type-face))

(defvar ebuild-mode-keywords-font
  '(("font_cleanup_dirs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain-binutils
  '(("is_cross" "add_src_uri" "tc-binutils_unpack"
     "tc-binutils_apply_patches")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain-funcs
  '(("tc-getPROG" "tc-getBUILD_PROG" "tc-getAR" "tc-getAS" "tc-getCC" "tc-getCPP" "tc-getCXX"
     "tc-getLD" "tc-getSTRIP" "tc-getNM" "tc-getRANLIB" "tc-getF77"
     "tc-getF90" "tc-getFORTRAN" "tc-getGCJ" "tc-getBUILD_CC" "tc-getBUILD_CPP"
     "tc-getBUILD_CXX" "tc-getRC" "tc-getDLLWRAP" "tc-getPKG_CONFIG"
     "tc-getBUILD_AR" "tc-getBUILD_AS" "tc-getBUILD_LD" "tc-getBUILD_STRIP"
     "tc-getBUILD_NM" "tc-getBUILD_RANLIB" "tc-getBUILD_OBJCOPY" "tc-getBUILD_PKG_CONFIG"
     "tc-export" "tc-is-cross-compiler" "tc-ninja_magic_to_arch" "tc-has-tls"
     "tc-is-hardfloat" "tc-arch-kernel" "tc-arch" "tc-endian"
     "gcc-fullversion" "gcc-version" "gcc-major-version" "gcc-minor-version"
     "gcc-micro-version")
    font-lock-type-face))

(defvar ebuild-mode-keywords-toolchain
  '(("is_crosscompile" "tc_version_is_at_least" "gcc_get_s_dir" "gentoo_urls"
     "get_gcc_src_uri" "get_make_var" "XGCC" "hardened_gcc_works"
     "hardened_gcc_is_stable" "hardened_gcc_check_unsupported" "has_libssp"
     "want_libssp" "want_boundschecking" "want_pie" "want_ssp"
     "want_split_specs" "glibc_have_pie" "libc_has_ssp" "gcc-lang-supported"
     "make_gcc_hard" "create_vanilla_specs_file" "create_hardened_specs_file"
     "create_hardenednossp_specs_file" "create_hardenednopie_specs_file"
     "create_hardenednopiessp_specs_file" "split_out_specs_files"
     "create_gcc_env_entry" "add_profile_eselect_conf" "create_eselect_conf"
     "guess_patch_type_in_dir" "do_gcc_rename_java_bins"
     "gcc-compiler-configure" "gcc_do_configure" "gcc_do_make" "gcc_do_filter_flags"
     "gcc_movelibs" "gcc_quick_unpack" "exclude_gcc_patches" "do_gcc_stub"
     "do_gcc_HTB_patches" "do_gcc_SSP_patches" "update_gcc_for_libc_ssp"
     "update_gcc_for_libssp" "do_gcc_PIE_patches" "should_we_gcc_config"
     "do_gcc_config" "should_we_eselect_compiler" "do_eselect_compiler"
     "gcc_version_patch" "setup_multilib_osdirnames" "gcc-multilib-configure"
     "disable_multilib_libjava" "fix_libtool_libdir_paths" "is_multilib"
     "is_cxx" "is_d" "is_f77" "is_f95" "is_fortran" "is_gcj" "is_objc"
     "is_objcxx" "is_ada")
    font-lock-type-face))

(defvar ebuild-mode-keywords-libtool
  '(("elibtoolize" "uclibctoolize" "darwintoolize" "ELT_find_ltmain_sh"
     "ELT_try_and_apply_patch" "ELT_libtool_version" "ELT_walk_patches"
     "elibtoolize" "VER_major" "VER_minor" "VER_micro" "VER_to_int"
     "elt_patch_dir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fixheadtails
  '(("ht_fix_file" "ht_fix_all")
    font-lock-type-face))

(defvar ebuild-mode-keywords-webapp
  '(("webapp_checkfileexists" "webapp_import_config" "webapp_strip_appdir"
     "webapp_strip_d" "webapp_strip_cwd" "webapp_configfile"
     "webapp_hook_script" "webapp_postinst_txt" "webapp_postupgrade_txt"
     "webapp_runbycgibin" "webapp_serverowned" "webapp_server_configfile"
     "webapp_sqlscript" "webapp_src_install" "webapp_pkg_postinst"
     "webapp_pkg_setup" "webapp_getinstalltype" "webapp_src_preinst"
     "webapp_pkg_prerm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-darcs
  '(("darcs_src_unpack" "darcs_patchcount")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-common-r1
  '(("php_check_cflags" "php_check_imap" "php_check_java" "php_install_java"
     "php_install_java_inifile" "php_check_mta" "php_check_oracle_all"
     "php_check_oracle_8" "php_check_pgsql" "php_get_mycnf_charset")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-ext-base-r1
  '(("php-ext-base-r1_buildinilist" "php-ext-base-r1_src_install"
     "php-ext-base-r1_addextension" "php-ext-base-r1_addtoinifile"
     "php-ext-base-r1_addtoinifiles")
    font-lock-type-face))

(defvar ebuild-mode-keywords-php-pear-r1
  '(("fix_PEAR_PV")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozilla-launcher
  '(("update_mozilla_launcher_symlinks" "install_mozilla_launcher_stub"
     "warn_mozilla_launcher_stub")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mozextensions
  '(("xpi_unpack" "xpi_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-db
  '(("db_fix_so" "db_src_install_usrbinslot" "db_src_install_headerslota"
     "db_src_install_usrlibcleanup")
    font-lock-type-face))

(defvar ebuild-mode-keywords-db-use
  '(("db_ver_to_slot" "db_findver" "db_includedir" "db_libname")
    font-lock-type-face))

(defvar ebuild-mode-keywords-confutils
  '(("confutils_init" "confutils_require_any" "confutils_use_conflict"
     "confutils_use_depend_all" "confutils_use_depend_any"
     "enable_extension_disable" "enable_extension_enable"
     "enable_extension_enableonly" "enable_extension_without"
     "enable_extension_with" "enable_extension_withonly"
     "confutils_warn_about_missing_deps" "enable_extension_enable_built_with"
     "enable_extension_with_built_with")
    font-lock-type-face))

(defvar ebuild-mode-keywords-linux-info
  '(("qout" "qeinfo" "qeerror" "getfilevar" "getfilevar_noexec"
     "linux_chkconfig_present" "linux_chkconfig_module"
     "linux_chkconfig_builtin" "linux_chkconfig_string" "kernel_is"
     "get_localversion" "get_version" "get_running_version"
     "check_kernel_built" "check_modules_supported" "check_extra_config"
     "check_zlibinflate" "linux-info_get_any_version"
     "get_makefile_extract_function")
    font-lock-type-face))

(defvar ebuild-mode-keywords-linux-mod
  '(("check_vermagic" "use_m" "convert_to_m" "update_depmod" "update_modules"
     "move_old_moduledb" "update_moduledb" "remove_moduledb" "set_kvobj"
     "get-KERNEL_CC" "generate_modulesd" "find_module_params"
     "strip_modulenames" "linux-mod_pkg_setup_binary")
    font-lock-type-face))

(defvar ebuild-mode-keywords-nsplugin
  '(("src_mv_plugins" "pkg_mv_plugins" "inst_plugin")
    font-lock-type-face))

(defvar ebuild-mode-keywords-latex-package
  '(("latex-package_has_tetex_3" "latex-package_src_doinstall"
     "latex-package_rehash" "latex-package_pkg_postinst"
     "latex-package_pkg_postrm" "latex-package_src_compile"
     "latex-package_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-texlive-module
  '(("etexmf-update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-texlive-module
  '(("texlive-module_make_language_def_lines"
     "texlive-module_make_language_dat_lines" "texlive-module_add_format"
     "texlive-module_synonyms_to_language_lua_line"
     "texlive-module_make_language_lua_lines")
    font-lock-type-face))

(defvar ebuild-mode-keywords-autotools
  '(("eautoreconf" "eaclocal" "_elibtoolize" "eautoconf" "eautoheader"
     "eautopoint" "eautomake" "autotools_m4dir_include" "eaclocal_amflags"
     "WANT_AUTOCONF" "WANT_AUTOMAKE")
    font-lock-type-face))

(defvar ebuild-mode-keywords-autotools-utils
  '(("myeconfargs" "remove_libtool_files")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qt4
  '(("qt4_min_version" "qt4_min_version_list")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qt4-r2
  '(("eqmake4")
    font-lock-type-face))

(defvar ebuild-mode-keywords-qt4-build
  '(("fix_includes" "setqtenv" "standard_configure_options"
     "build_directories" "install_directories" "install_qconfigs"
     "generate_qconfigs" "skip_qmake_build_patch"
     "skip_project_generation_patch" "symlink_binaries_to_buildtree"
     "fix_library_files" "qt_use" "qt_mkspecs_dir" "qt_assistant_cleanup"
     "qt_nolibx11" "prepare_directories")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-functions
  '(("buildsycoca" "comment_all_add_subdirectory" "enable_selected_linguas"
     "enable_selected_doc_linguas" "get_build_type" "migrate_store_dir"
     "save_library_dependencies" "install_library_dependencies"
     "load_library_dependencies" "slot_is_at_least" "add_blocker"
     "block_other_slots" "add_kdebase_dep")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-base
  '(("kde4-base_src_make_doc" "get_build_type")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kde4-meta
  '(("kde4-meta_create_extractlists" "kde4-meta_change_cmakelists"
     "kde4-meta_src_make_doc")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gst-plugins10
  '(("gst-plugins10_find_plugin_dir"
     "gst-plugins10_remove_unversioned_binaries")
    font-lock-type-face))

(defvar ebuild-mode-keywords-multilib
  '(("has_multilib_profile" "get_libdir" "get_abi_var" "get_install_abis" "get_all_abis"
     "get_all_libdirs" "is_final_abi" "number_abis" "get_libname" "multilib_env"
     "multilib_toolchain_setup" "get_abi_CFLAGS" "get_abi_LDFLAGS"
     "get_abi_CHOST" "get_abi_FAKE_TARGETS" "get_abi_LIBDIR")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mount-boot
  '(("mount-boot_mount_boot_partition" "mount-boot_umount_boot_partition")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-ant-2
  '(("java-ant_bsfix_files" "java-ant_bsfix_one" "java-ant_rewrite-classpath"
     "java-ant_ignore-system-classes" "java-ant_xml-rewrite"
     "java-ant_rewrite-bootclasspath" "java-pkg_get-bootclasspath"
     "java-ant_remove-taskdefs")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-vm-2
  '(("java-vm_check-nsplugin" "java-vm_sandbox-predict" "java-vm_set-nsplugin"
     "java_set_default_vm_" "get_system_arch" "set_java_env" "java-vm_set-pax-markings"
     "java-vm_revdep-mask" "java_get_plugin_dir_" "install_mozilla_plugin"
     "java_mozilla_clean_")
    font-lock-type-face))

(defvar ebuild-mode-keywords-java-utils-2
  '(("java-pkg_doexamples" "java-pkg_dojar" "java-pkg_regjar"
     "java-pkg_newjar" "java-pkg_addcp" "java-pkg_doso" "java-pkg_regso"
     "java-pkg_jarinto" "java-pkg_sointo" "java-pkg_dohtml"
     "java-pkg_dojavadoc" "java-pkg_dosrc" "java-pkg_dolauncher"
     "java-pkg_dowar" "java-pkg_jar-from" "java-pkg_jarfrom"
     "java-pkg_getjars" "java-pkg_getjar" "java-pkg_register-dependency"
     "java-pkg_register-optional-dependency"
     "java-pkg_register-environment-variable" "java-pkg_find-normal-jars"
     "java-pkg_ensure-no-bundled-jars" "java-pkg_get-source"
     "java-pkg_set-current-vm" "java-pkg_get-current-vm"
     "java-pkg_current-vm-matches" "java-pkg_get-target" "java-pkg_get-javac"
     "java-pkg_javac-args" "java-pkg_get-jni-cflags" "java-pkg_ensure-gcj"
     "java-pkg_ensure-test" "java-pkg_register-ant-task" "ejunit" "ejunit4" "eant"
     "ejavac" "java-pkg_filter-compiler" "java-pkg_force-compiler" "use_doc"
     "EANT_NEEDS_TOOLS")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bash-completion-r1
  '(("dobashcomp" "newbashcomp")
    font-lock-type-face))

(defvar ebuild-mode-keywords-fdo-mime
  '(("fdo-mime_desktop_database_update" "fdo-mime_mime_database_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome2-utils
  '(("gnome2_gconf_savelist" "gnome2_gconf_install" "gnome2_gconf_uninstall"
     "gnome2_icon_savelist" "gnome2_icon_cache_update" "gnome2_omf_fix"
     "gnome2_scrollkeeper_update" "gnome2_schemas_savelist"
     "gnome2_schemas_update")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gnome2
  '(("gnome2_src_configure" "gnome2_src_compile" "gnome2_src_install"
     "gnome2_src_unpack" "gnome2_gconf_uninstall" "gnome2_pkg_postinst"
     "gnome2_pkg_postrm" "DOCS")
    font-lock-type-face))

(defvar ebuild-mode-keywords-alternatives
  '(("alternatives_pkg_postinst" "alternatives_pkg_postrm"
     "alternatives_makesym" "alternatives_auto_makesym")
    font-lock-type-face))

(defvar ebuild-mode-keywords-eutils
  '(("epause" "ebeep" "epatch" "emktemp" "enewuser" "enewgroup" "edos2unix"
     "make_desktop_entry" "validate_desktop_entries" "make_session_desktop"
     "domenu" "newmenu" "doicon" "newicon" "check_license" "cdrom_get_cds"
     "cdrom_load_next_cd" "strip-linguas" "set_arch_to_kernel" "in_iuse"
     "set_arch_to_portage" "preserve_old_lib" "preserve_old_lib_notify"
     "built_with_use" "epunt_cxx" "make_wrapper" "gen_usr_ldscript" "usex"
     "draw_line" "have_NPTL" "get_number_of_jobs" "egetent" "unpack_pdv"
     "unpack_makeself" "cdrom_load_next" "cdrom_locate_file_on_cd"
     "find_unpackable_file" "eshopts_push" "eshopts_pop" "path_exists")
    font-lock-type-face))

(defvar ebuild-mode-keywords-pam
  '(("dopamd" "newpamd" "dopamsecurity" "newpamsecurity" "getpam_mod_dir"
     "dopammod" "newpammod" "pamd_mimic" "clean_pamd" "pammod_hide_symbols")
    font-lock-type-face))

(defvar ebuild-mode-keywords-haskell-cabal
  '(("cabal-hscolour-haddock" "cabal-hscolour" "cabal-version"
     "cabal-bootstrap" "cabal-mksetup" "cabal-haddock" "cabal-configure"
     "cabal-build" "cabal-copy" "cabal-pkg" "cabal-is-dummy-lib" "cabal_flag")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql
  '(("bitkeeper_fetch" "mysql_disable_test" "mysql_init_vars"
     "configure_minimal" "configure_common" "configure_40_41_50"
     "configure_51" "xtradb_applicable" "pbxt_applicable" "pbxt_available")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mysql_fx
  '(("stripdots" "mysql_check_version_range" "mysql_mv_patches"
     "mysql_version_is_at_least" "mysql_lib_symlinks")
    font-lock-type-face))

(defvar ebuild-mode-keywords-bzr
  '(("bzr_src_unpack" "bzr_fetch" "bzr_bootstrap")
    font-lock-type-face))

(defvar ebuild-mode-keywords-cvs
  '(("cvs_src_unpack" "cvs_fetch")
    font-lock-type-face))

(defvar ebuild-mode-keywords-subversion
  '(("subversion_src_unpack" "subversion_fetch" "subversion_bootstrap")
    font-lock-type-face))

(defvar ebuild-mode-keywords-git
  '(("git_src_unpack" "git_fetch" "git_bootstrap" "git_submodules")
    font-lock-type-face))

(defvar ebuild-mode-keywords-mercurial
  '(("mercurial_src_unpack" "mercurial_fetch")
    font-lock-type-face))

(defvar ebuild-mode-keywords-rpm
  '(("rpm_src_unpack" "rpm_spec_epatch")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim
  '(("apply_vim_patches" "update_vim_symlinks")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-doc
  '(("update_vim_helptags")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vim-plugin
  '(("vim-plugin_src_install" "vim-plugin_pkg_postinst"
     "vim-plugin_pkg_postrm" "update_vim_afterscripts"
     "display_vim_plugin_help")
    font-lock-type-face))

(defvar ebuild-mode-keywords-sandbox
  '(("adddeny" "addpredict" "addread" "addwrite")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-eclass
  '(("inherit")
    font-lock-type-face))

(defvar ebuild-mode-keywords-flag-o-matic
  '(("filter-flags"  "filter-lfs-flags"  "append-cppflags" "append-cflags"
     "append-cxxflags"  "append-fflags" "append-lfs-flags"  "append-flags"
     "replace-flags" "replace-cpu-flags" "is-flagq" "is-flag" "is-ldflagq"
     "is-ldflag" "filter-mfpmath" "strip-flags" "test-flag-CC" "test-flag-CXX"
     "test-flag-F77" "test-flag-FC" "test-flags-CC" "test-flags-CXX"
     "test-flags-F77" "test-flags-FC" "test-flags" "test_version_info"
     "strip-unsupported-flags" "get-flag" "has_m64" "has_m32"
     "replace-sparc64-flags" "append-libs" "append-ldflags" "filter-ldflags"
     "raw-ldflags" "no-as-needed")
    font-lock-type-face))

(defvar ebuild-mode-keywords-python
  '(("NEED_PYTHON" "DOCS" "PYTHON" "python_version"
     "python_mod_optimize" "python_mod_cleanup" "python_disable_pyc"
     "python_enable_pyc" "python_get_libdir" "python_get_sitedir"
     "python_need_rebuild" "python_copy_sources" "python_execute_function"
     "python_get_includedir" "python_convert_shebangs" "python_abi_depend"
     "python_set_active_version" "python_generate_wrapper_scripts"
     "python_get_library" "python_get_version" "python_execute_nosetest"
     "python_execute_py.test" "python_execute_trial"
     "python_get_implementation" "python_get_implementational_package"
     "python_clean_installation_image"
     "python_merge_intermediate_installation_images")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common-3
  '(("do-debian-credits" "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common-2
  '(("do-debian-credits" "standard-impl-postinst" "standard-impl-postrm")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp-common
  '(("do-debian-credits" "standard-impl-postinst" "standard-impl-postrm"
     "register-common-lisp-implementation"
     "unregister-common-lisp-implementation"
     "reregister-all-common-lisp-implementations")
    font-lock-type-face))

(defvar ebuild-mode-keywords-common-lisp
  ;; common-lisp-system-symlink is obsolete
  '(("common-lisp-symlink-asdf" "common-lisp-system-symlink"
     "common-lisp-install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby
  '(("ruby_econf" "ruby_emake" "doruby" "ruby_einstall" "erubydoc" "erubyconf"
     "erubymake" "erubyinstall" "RUBY_OPTIONAL")
    font-lock-type-face))

(defvar ebuild-mode-keywords-gems
  '(("gems_src_install" "gems-location")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-fakegem
  '(("ruby_fakegem_gemsdir" "ruby_fakegem_doins" "ruby_fakegem_newsins"
     "ruby_fakegem_genspec" "ruby_fakegem_binwrapper"
     "ruby_fakegem_install_gemspec" "ruby_fakegem_metadata_gemspec"
     "all_fakegem_compile" "all_fakegem_install" "all_ruby_install"
     "all_ruby_unpack" "all_ruby_compile" "each_fakegem_test" "each_ruby_test"
     "each_fakegem_install" "each_ruby_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-ng
  '(("ruby_implementation_depend" "ruby_samelib" "ruby_add_rdepend"
     "ruby_add_bdepend" "doruby" "ruby_get_libruby" "ruby_get_hdrdir"
     "ruby_get_version" "ruby_get_implementation" "ruby_rbconfig_value")
    font-lock-type-face))

(defvar ebuild-mode-keywords-ruby-ng-gnome2
  '(("each_ruby_configure" "each_ruby_compile" "each_ruby_install"
     "all_ruby_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-elisp-common
  '(("elisp-compile" "elisp-emacs-version" "elisp-need-emacs"
     "elisp-make-autoload-file" "elisp-install" "elisp-site-file-install"
     "elisp-site-regen")
    font-lock-type-face))

(defvar ebuild-mode-keywords-elisp
  '(("NEED_EMACS" "ELISP_PATCHES" "ELISP_TEXINFO" "SITEFILE" "DOCS"
     "elisp_pkg_setup" "elisp_pkg_postinst" "elisp_pkg_postrm"
     "elisp_src_unpack" "elisp_src_prepare" "elisp_src_configure"
     "elisp_src_compile" "elisp_src_install")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games-mods
  '(("default_client" "games-mods_make_initd" "games-mods_make_confd")
    font-lock-type-face))

(defvar ebuild-mode-keywords-games
  '(("games_get_libdir" "egamesconf" "egamesinstall" "gameswrapper"
     "dogamesbin" "dogamessbin" "dogameslib" "dogameslib.a" "dogameslib.so"
     "newgamesbin" "newgamessbin" "games_make_wrapper" "gamesowners"
     "gamesperms" "prepgamesdirs" "gamesenv" "games_ut_unpack"
     "games_umod_unpack" "games_link_mods" "games_ut_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-perl-app
  '(("perl-app_src_prep")
    font-lock-type-face))

(defvar ebuild-mode-keywords-perl-helper
  '(("perlinfo" "perl_set_version" "fixlocalpod" "perl_delete_localpod"
     "perl_fix_osx_extra" "perl_delete_module_manpages" "perl_delete_packlist"
     "perl_remove_temppath" "perl_link_duallife_scripts" "perl_set_eprefix")
    font-lock-type-face))

(defvar ebuild-mode-keywords-distutils
  '(("distutils_python_version" "distutils_python_tkinter"
     "distutils_src_unpack")
    font-lock-type-face))

(defvar ebuild-mode-keywords-scons-utils
  '(("escons" "scons_clean_makeopts" "use_scons")
    font-lock-type-face))

(defvar ebuild-mode-keywords-depend-apache
  '(("need_apache" "need_apache1" "need_apache2")
    font-lock-type-face))

(defvar ebuild-mode-keywords-apache-module
  '(("apache-module_pkg_setup" "apache-module_src_compile"
     "apache-module_src_install" "apache-module_pkg_postinst" "apache_cd_dir"
     "apache_mod_file" "apache_doc_magic")
    font-lock-type-face))

(defvar ebuild-mode-keywords-kernel-2
  '(("debug-print-kernel2-variables" "handle_genpatches" "detect_version"
     "kernel_is" "kernel_is_2_4" "kernel_is_2_6" "kernel_header_destdir"
     "cross_pre_c_headers" "env_setup_xmakeopts" "unpack_2_4" "unpack_2_6"
     "universal_unpack" "unpack_set_extraversion" "unpack_fix_install_path"
     "compile_headers" "compile_headers_tweak_config" "install_universal"
     "install_headers" "install_sources" "preinst_headers" "postinst_sources"
     "postinst_headers" "setup_headers" "unipatch" "getfilevar" "detect_arch"
     "generate_sparc_asm" "headers___fix")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xorg-2
  '(("xorg-2_patch_source" "xorg-2_reconf_source" "xorg-2_font_configure"
     "xorg-2_flags_setup" "setup_fonts" "remove_font_metadata"
     "create_fonts_scale" "create_fonts_dir")
    font-lock-type-face))

(defvar ebuild-mode-keywords-xfconf
  '(("xfconf_use_debug")
    font-lock-type-face))

(defvar ebuild-mode-keywords-versionator
  '(("get_all_version_components" "get_version_components" "get_major_version"
     "get_version_component_range" "get_after_major_version"
     "replace_version_separator" "replace_all_version_separators"
     "delete_version_separator" "delete_all_version_separators"
     "get_version_component_count" "get_last_version_component_index"
     "version_is_at_least" "version_compare" "version_sort"
     "version_format_string")
    font-lock-type-face))

(defvar ebuild-mode-keywords-vmware-bundle
  '(("vmware-bundle_extract-bundle-component" "vmware-bundle_extract-component")
    font-lock-type-face))

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; no-byte-compile: t
;; End:

;;; ebuild-mode-keywords.el ends here
