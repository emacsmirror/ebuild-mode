;;; ebuild-mode-keywords.el --- keywords for font-lock  -*-lexical-binding:t-*-

;; Copyright 2006-2024 Gentoo Authors

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

;; Package manager keywords

(defvar ebuild-mode-keywords-EAPI
  ;; highlight the EAPI variable itself
  '(("EAPI")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-0
  '(("assert" "best_version" "debug-print" "debug-print-function"
     "debug-print-section" "die" "diropts" "dobin" "docinto" "doconfd" "dodir"
     "dodoc" "doenvd" "doexe" "doinfo" "doinitd" "doins" "dolib.a" "dolib.so"
     "doman" "domo" "dosbin" "dosym" "ebegin" "econf" "eend" "eerror" "einfo"
     "einfon" "elog" "emake" "ewarn" "exeinto" "exeopts" "EXPORT_FUNCTIONS"
     "fowners" "fperms" "has" "has_version" "inherit" "insinto" "insopts"
     "into" "keepdir" "newbin" "newconfd" "newdoc" "newenvd" "newexe"
     "newinitd" "newins" "newlib.a" "newlib.so" "newman" "newsbin" "unpack"
     "use" "usev" "use_enable" "use_with"
     ;; EAPI 2
     "default" "default_pkg_nofetch" "default_src_unpack"
     "default_src_prepare" "default_src_configure" "default_src_compile"
     "default_src_test"
     ;; EAPI 4
     "default_src_install" "docompress" "nonfatal"
     ;; EAPI 5
     "doheader" "newheader" "usex"
     ;; EAPI 6
     "eapply" "eapply_user" "einstalldocs" "get_libdir" "in_iuse"
     ;; EAPI 7
     "dostrip" "eqawarn" "ver_cut" "ver_rs" "ver_test")
    font-lock-builtin-face))

(defvar ebuild-mode-keywords-functions
  '(("pkg_nofetch" "pkg_setup" "src_unpack" "src_compile" "src_test"
     "src_install" "pkg_preinst" "pkg_postinst" "pkg_prerm" "pkg_postrm"
     "pkg_config"
     ;; EAPI 2
     "pkg_info" "src_prepare" "src_configure"
     ;; EAPI 4
     "pkg_pretend")
    font-lock-type-face))

(defvar ebuild-mode-keywords-sandbox
  '(("adddeny" "addpredict" "addread" "addwrite")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-eapi-deprecated
  ;; deprecated or banned package manager commands
  '(("dohard" "dohtml" "dolib" "dosed" "einstall" "hasq" "hasv" "libopts"
     "portageq" "prepall" "prepalldocs" "prepallinfo" "prepallman"
     "prepallstrip" "prepinfo" "preplib" "prepman" "prepstrip" "useq")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-warn
  ;; warn about "which" usage, see <200703121910.26067.vapier@gentoo.org>
  ;; https://archives.gentoo.org/gentoo-dev/message/e04d4db72572dd5fec48e87c6b18c525
  '(("which")
    font-lock-warning-face))

;; Eclass keywords

(defvar ebuild-mode-keywords-eclassdoc
  '(("@AUTHOR" "@BLURB" "@BUGREPORTS" "@CODE" "@DEFAULT_UNSET" "@DESCRIPTION"
     "@ECLASS" "@ECLASS_VARIABLE" "@EXAMPLE" "@FUNCTION" "@INTERNAL"
     "@MAINTAINER" "@OUTPUT_VARIABLE" "@PRE_INHERIT" "@PROVIDES" "@REQUIRED"
     "@RETURN" "@ROFF" "@SUBSECTION" "@SUPPORTED_EAPIS" "@USAGE"
     "@USER_VARIABLE" "@VARIABLE" "@VCSURL")
    (1 font-lock-type-face t)
    "^# "))

(defvar ebuild-mode-keywords-eclassdoc-warn
  ;; @ECLASS-VARIABLE (with a hyphen) is deprecated:
  ;; https://bugs.gentoo.org/835396
  '(("@DEAD" "@DEPRECATED" "@ECLASS-VARIABLE")
    (1 font-lock-warning-face t)
    "^# "))

;; The list of eclass function keywords below is auto-generated
;; by keyword-generation.sh.

(defvar ebuild-mode-keywords-eclass
  '((
     ;; @@KEYWORDS-BEGIN@@
     ;; acct-group
     "acct-group_pkg_preinst" "acct-group_pkg_pretend"
     "acct-group_src_install"
     ;; acct-user
     "acct-user_add_deps" "acct-user_pkg_postinst" "acct-user_pkg_preinst"
     "acct-user_pkg_prerm" "acct-user_pkg_pretend" "acct-user_src_install"
     ;; ada
     "ada_export" "ada_pkg_setup" "ada_setup" "ada_wrapper_setup"
     ;; alternatives
     "alternatives_auto_makesym" "alternatives_makesym"
     "alternatives_pkg_postinst" "alternatives_pkg_postrm"
     ;; apache-2
     "apache-2_pkg_postinst" "apache-2_pkg_setup" "apache-2_src_configure"
     "apache-2_src_install" "apache-2_src_prepare" "check_module_critical"
     "check_upgrade" "generate_load_module" "setup_modules" "setup_mpm"
     ;; apache-module
     "apache-module_pkg_postinst" "apache-module_src_compile"
     "apache-module_src_install" "apache_cd_dir" "apache_doc_magic"
     "apache_mod_file"
     ;; app-alternatives
     "get_alternative"
     ;; aspell-dict-r1
     "aspell-dict-r1_src_configure" "aspell-dict-r1_src_install"
     ;; autotools
     "autotools_check_macro" "autotools_m4dir_include"
     "autotools_m4sysdir_include" "config_rpath_update" "eaclocal"
     "eaclocal_amflags" "eautoconf" "eautoheader" "eautomake" "eautopoint"
     "eautoreconf"
     ;; bash-completion-r1
     "bashcomp_alias" "dobashcomp" "get_bashcompdir" "newbashcomp"
     ;; cargo
     "cargo_crate_uris" "cargo_env" "cargo_gen_config" "cargo_live_src_unpack"
     "cargo_src_compile" "cargo_src_configure" "cargo_src_install"
     "cargo_src_test" "cargo_src_unpack" "cargo_target_dir"
     ;; cdrom
     "cdrom_get_cds" "cdrom_load_next_cd"
     ;; check-reqs
     "check-reqs_pkg_pretend" "check-reqs_pkg_setup"
     ;; chromium-2
     "chromium_pkg_die" "chromium_remove_language_paks"
     "chromium_suid_sandbox_check_kernel_config"
     ;; cmake
     "cmake_build" "cmake_comment_add_subdirectory" "cmake_run_in"
     "cmake_src_compile" "cmake_src_configure" "cmake_src_install"
     "cmake_src_prepare" "cmake_src_test" "cmake_use_find_package"
     ;; cmake-multilib
     "cmake-multilib_src_compile" "cmake-multilib_src_configure"
     "cmake-multilib_src_install" "cmake-multilib_src_test"
     "multilib_src_compile" "multilib_src_configure" "multilib_src_install"
     "multilib_src_test"
     ;; common-lisp-3
     "absolute-path-p" "common-lisp-3_src_compile" "common-lisp-3_src_install"
     "common-lisp-export-impl-args" "common-lisp-find-lisp-impl"
     "common-lisp-get-fpredicate" "common-lisp-install-asdf"
     "common-lisp-install-one-asdf" "common-lisp-install-one-source"
     "common-lisp-install-sources" "lisp-file-p"
     ;; cron
     "cron_pkg_postinst" "docron" "docrondir" "docrontab"
     ;; crossdev
     "is_crosspkg" "target_is_not_host"
     ;; cuda
     "cuda_add_sandbox" "cuda_cudnn_version" "cuda_gccdir" "cuda_sanitize"
     "cuda_src_prepare" "cuda_toolkit_version"
     ;; cvs
     "cvs_fetch" "cvs_src_unpack"
     ;; db
     "db_fix_so" "db_src_install_doc" "db_src_install_examples"
     "db_src_install_headerslot" "db_src_install_usrbinslot"
     "db_src_install_usrlibcleanup" "db_src_test"
     ;; db-use
     "db_findver" "db_includedir" "db_libname" "db_ver_to_slot"
     ;; depend.apache
     "depend.apache_pkg_setup" "has_apache" "has_apache_threads"
     "has_apache_threads_in" "need_apache" "need_apache2" "need_apache2_2"
     "need_apache2_4" "want_apache" "want_apache2" "want_apache2_2"
     "want_apache2_4"
     ;; desktop
     "doicon" "domenu" "make_desktop_entry" "make_session_desktop" "newicon"
     "newmenu"
     ;; dist-kernel-utils
     "dist-kernel_PV_to_KV" "dist-kernel_compressed_module_cleanup"
     "dist-kernel_get_image_path" "dist-kernel_get_module_suffix"
     "dist-kernel_install_kernel" "dist-kernel_reinstall_initramfs"
     ;; distutils-r1
     "distutils-r1_python_compile" "distutils-r1_python_install"
     "distutils-r1_python_install_all" "distutils-r1_python_prepare_all"
     "distutils-r1_python_test" "distutils-r1_src_compile"
     "distutils-r1_src_configure" "distutils-r1_src_install"
     "distutils-r1_src_prepare" "distutils-r1_src_test"
     "distutils_enable_sphinx" "distutils_enable_tests"
     "distutils_install_for_testing" "distutils_pep517_install"
     "distutils_wheel_install" "distutils_write_namespace" "esetup.py"
     ;; docs
     "docs_compile" "doxygen_compile" "initialize_git_repo" "mkdocs_compile"
     "sphinx_compile"
     ;; dotnet
     "dotnet_multilib_comply" "dotnet_pkg_setup" "egacinstall" "exbuild"
     ;; dotnet-pkg
     "dotnet-pkg_force-compat" "dotnet-pkg_foreach-project"
     "dotnet-pkg_pkg_setup" "dotnet-pkg_remove-bad" "dotnet-pkg_src_compile"
     "dotnet-pkg_src_configure" "dotnet-pkg_src_install"
     "dotnet-pkg_src_prepare" "dotnet-pkg_src_test" "dotnet-pkg_src_unpack"
     ;; dotnet-pkg-base
     "dotnet-pkg-base_append-launchervar" "dotnet-pkg-base_append_launchervar"
     "dotnet-pkg-base_build" "dotnet-pkg-base_dolauncher"
     "dotnet-pkg-base_dolauncher-portable"
     "dotnet-pkg-base_dolauncher_portable" "dotnet-pkg-base_foreach-solution"
     "dotnet-pkg-base_get-configuration" "dotnet-pkg-base_get-output"
     "dotnet-pkg-base_get-runtime" "dotnet-pkg-base_info"
     "dotnet-pkg-base_install" "dotnet-pkg-base_launcherinto"
     "dotnet-pkg-base_remove-global-json" "dotnet-pkg-base_restore"
     "dotnet-pkg-base_restore-tools" "dotnet-pkg-base_restore_tools"
     "dotnet-pkg-base_setup" "dotnet-pkg-base_sln-remove"
     "dotnet-pkg-base_test" "edotnet"
     ;; dune
     "dune-compile" "dune-install" "dune-release" "dune-test"
     "dune_src_compile" "dune_src_install" "dune_src_test" "edune"
     ;; eapi8-dosym
     "dosym8"
     ;; ecm
     "ecm_pkg_postinst" "ecm_pkg_postrm" "ecm_pkg_preinst" "ecm_pkg_pretend"
     "ecm_pkg_setup" "ecm_punt_bogus_dep" "ecm_punt_kf_module"
     "ecm_punt_po_install" "ecm_punt_qt_module" "ecm_src_compile"
     "ecm_src_configure" "ecm_src_install" "ecm_src_prepare" "ecm_src_test"
     ;; ecm-common
     "ecm-common-check_deps" "ecm-common_inject_heredoc"
     "ecm-common_pkg_setup" "ecm-common_src_configure"
     "ecm-common_src_prepare"
     ;; edo
     "edo" "edob"
     ;; edos2unix
     "edos2unix"
     ;; elisp
     "elisp_pkg_postinst" "elisp_pkg_postrm" "elisp_pkg_setup"
     "elisp_src_compile" "elisp_src_configure" "elisp_src_install"
     "elisp_src_prepare" "elisp_src_test" "elisp_src_unpack"
     ;; elisp-common
     "elisp-check-emacs-version" "elisp-compile" "elisp-emacs-version"
     "elisp-enable-tests" "elisp-install" "elisp-make-autoload-file"
     "elisp-make-site-file" "elisp-modules-install" "elisp-org-export-to"
     "elisp-site-file-install" "elisp-site-regen" "elisp-test"
     "elisp-test-buttercup" "elisp-test-ert" "elisp-test-ert-runner"
     ;; emboss-r3
     "emboss-r3_src_configure" "emboss-r3_src_install"
     ;; estack
     "eshopts_pop" "eshopts_push" "estack_pop" "estack_push" "eumask_pop"
     "eumask_push" "evar_pop" "evar_push" "evar_push_set"
     ;; fcaps
     "fcaps" "fcaps_pkg_postinst"
     ;; findlib
     "check_ocamlfind" "findlib_src_install" "findlib_src_preinst"
     ;; fixheadtails
     "ht_fix_all" "ht_fix_file"
     ;; flag-o-matic
     "all-flag-vars" "append-atomic-flags" "append-cflags" "append-cppflags"
     "append-cxxflags" "append-fflags" "append-flags" "append-ldflags"
     "append-lfs-flags" "append-libs" "filter-flags" "filter-ldflags"
     "filter-lfs-flags" "filter-lto" "filter-mfpmath" "get-flag" "is-flag"
     "is-flagq" "is-ldflag" "is-ldflagq" "no-as-needed" "raw-ldflags"
     "replace-cpu-flags" "replace-flags" "replace-sparc64-flags" "strip-flags"
     "strip-unsupported-flags" "test-compile" "test-flag-CC" "test-flag-CCLD"
     "test-flag-CXX" "test-flag-F77" "test-flag-FC" "test-flags"
     "test-flags-CC" "test-flags-CCLD" "test-flags-CXX" "test-flags-F77"
     "test-flags-FC" "test_version_info"
     ;; font
     "font_cleanup_dirs" "font_fontconfig" "font_pkg_postinst"
     "font_pkg_postrm" "font_pkg_setup" "font_src_install"
     "font_wrap_opentype_compat" "font_xfont_config"
     ;; font-ebdftopcf
     "ebdftopcf" "font-ebdftopcf_src_compile"
     ;; fortran-2
     "fortran-2_pkg_setup" "fortran_int64_abi_fflags"
     ;; freedict
     "freedict_src_install"
     ;; gap-pkg
     "gap-pkg_dir" "gap-pkg_econf" "gap-pkg_enable_tests"
     "gap-pkg_src_compile" "gap-pkg_src_configure" "gap-pkg_src_install"
     "gap-pkg_src_test"
     ;; ghc-package
     "check-for-collisions" "ghc-bindir" "ghc-cabal-version" "ghc-confdir"
     "ghc-extract-pm-version" "ghc-getghc" "ghc-getghcpkg" "ghc-getghcpkgbin"
     "ghc-install-pkg" "ghc-is-dynamic" "ghc-libdir" "ghc-localpkgconfd"
     "ghc-make-args" "ghc-package-db" "ghc-package-exists"
     "ghc-package_pkg_postinst" "ghc-package_pkg_postrm"
     "ghc-package_pkg_prerm" "ghc-pkgdeps" "ghc-pm-version" "ghc-recache-db"
     "ghc-register-pkg" "ghc-reregister" "ghc-supports-interpreter"
     "ghc-supports-parallel-make" "ghc-supports-shared-libraries"
     "ghc-supports-smp" "ghc-supports-threaded-runtime" "ghc-unregister-pkg"
     "ghc-version"
     ;; git-r3
     "git-r3_checkout" "git-r3_fetch" "git-r3_peek_remote_ref"
     "git-r3_pkg_needrebuild" "git-r3_src_fetch" "git-r3_src_unpack"
     "pkg_needrebuild"
     ;; gkrellm-plugin
     "gkrellm-plugin_src_install"
     ;; gnome2
     "gnome2_pkg_postinst" "gnome2_pkg_postrm" "gnome2_pkg_preinst"
     "gnome2_src_compile" "gnome2_src_configure" "gnome2_src_install"
     "gnome2_src_prepare"
     ;; gnome2-utils
     "gnome2_disable_deprecation_warning" "gnome2_environment_reset"
     "gnome2_gconf_install" "gnome2_gconf_savelist" "gnome2_gconf_uninstall"
     "gnome2_gdk_pixbuf_savelist" "gnome2_gdk_pixbuf_update"
     "gnome2_giomodule_cache_update" "gnome2_omf_fix"
     "gnome2_query_immodules_gtk2" "gnome2_query_immodules_gtk3"
     "gnome2_schemas_savelist" "gnome2_schemas_update"
     "gnome2_scrollkeeper_savelist" "gnome2_scrollkeeper_update"
     ;; gnuconfig
     "gnuconfig_update"
     ;; gnustep-base
     "egnustep_doc" "egnustep_env" "egnustep_install"
     "egnustep_install_config" "egnustep_make" "gnustep-base_pkg_postinst"
     "gnustep-base_pkg_setup" "gnustep-base_src_compile"
     "gnustep-base_src_configure" "gnustep-base_src_install"
     "gnustep-base_src_prepare"
     ;; go-env
     "go-env_go386" "go-env_goarch" "go-env_goarm" "go-env_goos"
     "go-env_set_compile_environment"
     ;; go-module
     "ego" "go-module_live_vendor" "go-module_set_globals"
     "go-module_setup_proxy" "go-module_src_unpack"
     ;; golang-base
     "ego_pn_check" "get_golibdir" "get_golibdir_gopath" "golang_install_pkgs"
     ;; golang-build
     "golang-build_src_compile" "golang-build_src_install"
     "golang-build_src_test"
     ;; golang-vcs
     "golang-vcs_src_fetch" "golang-vcs_src_unpack"
     ;; golang-vcs-snapshot
     "golang-vcs-snapshot_src_unpack"
     ;; greadme
     "greadme_file" "greadme_pkg_postinst" "greadme_pkg_preinst"
     "greadme_stdin"
     ;; gstreamer-meson
     "gstreamer-meson_pkg_setup" "gstreamer_multilib_src_compile"
     "gstreamer_multilib_src_configure" "gstreamer_multilib_src_install"
     "gstreamer_multilib_src_install_all" "gstreamer_multilib_src_test"
     "gstreamer_system_library" "gstreamer_system_package"
     "multilib_src_compile" "multilib_src_configure" "multilib_src_install"
     "multilib_src_test"
     ;; guile
     "guile_copy_sources" "guile_for_best_impl" "guile_foreach_impl"
     "guile_merge_roots" "guile_pkg_setup" "guile_src_compile"
     "guile_src_configure" "guile_src_install" "guile_src_prepare"
     "guile_src_test"
     ;; guile-single
     "guile-single_pkg_setup" "guile-single_src_install"
     "guile-single_src_prepare" "guile_gen_cond_dep"
     ;; guile-utils
     "guile_bump_sources" "guile_check_compat" "guile_create_temporary_config"
     "guile_export" "guile_filter_pkgconfig_path" "guile_generate_depstrings"
     "guile_set_common_vars" "guile_unstrip_ccache"
     ;; haskell-cabal
     "cabal-bootstrap" "cabal-build" "cabal-configure" "cabal-constraint"
     "cabal-copy" "cabal-die-if-nonempty" "cabal-export-dist-libs"
     "cabal-haddock" "cabal-hscolour" "cabal-is-dummy-lib" "cabal-mksetup"
     "cabal-pkg" "cabal-register-inplace" "cabal-run-dist-bin"
     "cabal-show-brokens" "cabal-show-brokens-and-die" "cabal-show-old"
     "cabal-version" "cabal_chdeps" "cabal_flag" "cabal_src_compile"
     "cabal_src_configure" "cabal_src_install" "haskell-cabal-run_verbose"
     "haskell-cabal_pkg_postinst" "haskell-cabal_pkg_postrm"
     "haskell-cabal_pkg_setup" "haskell-cabal_src_compile"
     "haskell-cabal_src_configure" "haskell-cabal_src_install"
     "haskell-cabal_src_prepare" "haskell-cabal_src_test" "replace-hcflags"
     ;; java-ant-2
     "java-ant-2_src_configure" "java-ant_bsfix_files" "java-ant_bsfix_one"
     "java-ant_ignore-system-classes" "java-ant_rewrite-bootclasspath"
     "java-ant_rewrite-classpath" "java-ant_xml-rewrite"
     ;; java-osgi
     "java-osgi_dojar" "java-osgi_dojar-fromfile" "java-osgi_newjar"
     "java-osgi_newjar-fromfile"
     ;; java-pkg-2
     "java-pkg-2_pkg_preinst" "java-pkg-2_pkg_setup" "java-pkg-2_src_compile"
     "java-pkg-2_src_prepare" "java-pkg-2_src_test"
     ;; java-pkg-opt-2
     "java-pkg-opt-2_pkg_preinst" "java-pkg-opt-2_pkg_setup"
     "java-pkg-opt-2_src_prepare"
     ;; java-pkg-simple
     "java-pkg-simple_src_compile" "java-pkg-simple_src_install"
     "java-pkg-simple_src_test"
     ;; java-utils-2
     "eant" "ejavac" "ejavadoc" "ejunit" "ejunit4" "increment-qa-violations"
     "is-java-strict" "java-pkg_addcp" "java-pkg_addres"
     "java-pkg_announce-qa-violation" "java-pkg_check-jikes"
     "java-pkg_check-phase" "java-pkg_check-versioned-jar" "java-pkg_clean"
     "java-pkg_current-vm-matches" "java-pkg_doexamples" "java-pkg_dohtml"
     "java-pkg_dojar" "java-pkg_dojavadoc" "java-pkg_dolauncher"
     "java-pkg_doso" "java-pkg_dosrc" "java-pkg_dowar"
     "java-pkg_ensure-no-bundled-jars" "java-pkg_filter-compiler"
     "java-pkg_find-normal-jars" "java-pkg_force-compiler"
     "java-pkg_get-bootclasspath" "java-pkg_get-javac"
     "java-pkg_get-jni-cflags" "java-pkg_get-source" "java-pkg_get-target"
     "java-pkg_getjar" "java-pkg_getjars" "java-pkg_init-compiler_"
     "java-pkg_jar-from" "java-pkg_jar-list" "java-pkg_jarfrom"
     "java-pkg_jarinto" "java-pkg_javac-args" "java-pkg_newjar"
     "java-pkg_register-ant-task" "java-pkg_register-dependency"
     "java-pkg_register-environment-variable"
     "java-pkg_register-optional-dependency" "java-pkg_regjar"
     "java-pkg_regso" "java-pkg_rm_files" "java-pkg_set-current-vm"
     "java-pkg_sointo" "java-utils-2_pkg_preinst" "java-utils-2_src_prepare"
     "use_doc"
     ;; java-vm-2
     "get_system_arch" "java-vm-2_pkg_postinst" "java-vm-2_pkg_postrm"
     "java-vm-2_pkg_prerm" "java-vm-2_pkg_setup" "java-vm_install-env"
     "java-vm_revdep-mask" "java-vm_sandbox-predict"
     "java-vm_set-pax-markings"
     ;; kde.org
     "kde.org_pkg_nofetch" "kde.org_src_unpack"
     ;; kernel-2
     "cross_pre_c_headers" "debug-print-kernel2-variables" "detect_arch"
     "detect_version" "env_setup_kernel_makeopts" "getfilevar"
     "handle_genpatches" "headers___fix" "install_headers" "install_sources"
     "install_universal" "kernel-2_pkg_postinst" "kernel-2_pkg_postrm"
     "kernel-2_pkg_preinst" "kernel-2_pkg_setup" "kernel-2_src_compile"
     "kernel-2_src_install" "kernel-2_src_prepare" "kernel-2_src_test"
     "kernel-2_src_unpack" "kernel_header_destdir" "kernel_is"
     "postinst_sources" "preinst_headers" "setup_headers" "unipatch"
     "universal_unpack" "unpack_fix_install_path" "unpack_set_extraversion"
     ;; kernel-build
     "kernel-build_merge_configs" "kernel-build_pkg_postinst"
     "kernel-build_pkg_setup" "kernel-build_src_compile"
     "kernel-build_src_configure" "kernel-build_src_install"
     "kernel-build_src_test"
     ;; kernel-install
     "kernel-install_can_update_symlink" "kernel-install_compress_modules"
     "kernel-install_create_init" "kernel-install_create_qemu_image"
     "kernel-install_extract_from_uki" "kernel-install_get_qemu_arch"
     "kernel-install_install_all" "kernel-install_pkg_config"
     "kernel-install_pkg_postinst" "kernel-install_pkg_postrm"
     "kernel-install_pkg_preinst" "kernel-install_pkg_pretend"
     "kernel-install_src_test" "kernel-install_test"
     "kernel-install_update_symlink"
     ;; kodi-addon
     "kodi-addon_src_configure"
     ;; latex-package
     "latex-package_pkg_postinst" "latex-package_pkg_postrm"
     "latex-package_rehash" "latex-package_src_compile"
     "latex-package_src_doinstall" "latex-package_src_install"
     ;; libretro-core
     "libretro-core_src_compile" "libretro-core_src_install"
     "libretro-core_src_prepare" "libretro-core_src_unpack"
     ;; libtool
     "elibtoolize"
     ;; linux-info
     "check_extra_config" "check_kernel_built" "check_modules_supported"
     "check_zlibinflate" "get_running_version" "get_version" "getfilevar"
     "getfilevar_noexec" "kernel_get_makefile" "kernel_is"
     "linux-info_get_any_version" "linux-info_pkg_setup"
     "linux_chkconfig_builtin" "linux_chkconfig_module"
     "linux_chkconfig_present" "linux_chkconfig_string"
     "linux_config_bin_exists" "linux_config_exists" "linux_config_path"
     "linux_config_src_exists" "qeerror" "qeinfo" "qewarn" "qout"
     "require_configured_kernel" "set_arch_to_kernel" "set_arch_to_pkgmgr"
     ;; linux-mod
     "convert_to_m" "get-KERNEL_CC" "linux-mod_pkg_postinst"
     "linux-mod_pkg_postrm" "linux-mod_pkg_preinst" "linux-mod_pkg_setup"
     "linux-mod_pkg_setup_binary" "linux-mod_src_compile"
     "linux-mod_src_install" "remove_moduledb" "set_kvobj" "strip_modulenames"
     "update_moduledb" "use_m"
     ;; linux-mod-r1
     "linux-mod-r1_pkg_postinst" "linux-mod-r1_pkg_setup"
     "linux-mod-r1_src_compile" "linux-mod-r1_src_install" "linux_domodule"
     "linux_moduleinto" "modules_post_process"
     ;; llvm
     "get_llvm_prefix" "get_llvm_slot" "llvm_pkg_setup"
     ;; llvm-r1
     "get_llvm_prefix" "llvm-r1_pkg_setup" "llvm_gen_dep"
     ;; llvm-utils
     "llvm_fix_clang_version" "llvm_fix_tool_path" "llvm_prepend_path"
     "llvm_tuple_to_target"
     ;; llvm.org
     "get_lit_flags" "llvm.org_set_globals" "llvm.org_src_prepare"
     "llvm.org_src_unpack" "llvm_are_manpages_built" "llvm_install_manpages"
     ;; lua
     "lua_copy_sources" "lua_foreach_impl"
     ;; lua-single
     "lua-single_pkg_setup" "lua_gen_cond_dep" "lua_gen_impl_dep" "lua_setup"
     ;; lua-utils
     "lua_enable_tests" "lua_get_CFLAGS" "lua_get_LIBS" "lua_get_cmod_dir"
     "lua_get_include_dir" "lua_get_lmod_dir" "lua_get_shared_lib"
     "lua_get_version"
     ;; mate
     "ematedocize" "mate_pkg_postinst" "mate_pkg_postrm" "mate_pkg_preinst"
     "mate_py_cond_func_wrap" "mate_src_configure" "mate_src_install"
     "mate_src_prepare" "want_mate_doc"
     ;; mercurial
     "mercurial_fetch" "mercurial_src_unpack"
     ;; meson
     "meson_feature" "meson_install" "meson_src_compile" "meson_src_configure"
     "meson_src_install" "meson_src_test" "meson_use"
     "setup_meson_src_configure"
     ;; meson-multilib
     "meson-multilib_src_compile" "meson-multilib_src_configure"
     "meson-multilib_src_install" "meson-multilib_src_test"
     "meson_native_enabled" "meson_native_true" "meson_native_use_bool"
     "meson_native_use_feature" "multilib_src_compile"
     "multilib_src_configure" "multilib_src_install" "multilib_src_test"
     ;; mono
     "egacinstall" "mono_multilib_comply"
     ;; mono-env
     "mono-env_pkg_setup"
     ;; mount-boot
     "mount-boot_pkg_postinst" "mount-boot_pkg_postrm"
     "mount-boot_pkg_preinst" "mount-boot_pkg_prerm" "mount-boot_pkg_pretend"
     ;; mozcoreconf-v6
     "moz_pkgsetup" "mozconfig_annotate" "mozconfig_final" "mozconfig_init"
     "mozconfig_use_enable" "mozconfig_use_with"
     ;; mozextension
     "mozversion_extension_location" "xpi_copy" "xpi_install" "xpi_unpack"
     ;; mozlinguas-v2
     "mozlinguas-v2_src_compile" "mozlinguas-v2_src_install"
     "mozlinguas-v2_src_unpack" "mozlinguas_mozconfig"
     "mozlinguas_src_compile" "mozlinguas_src_install" "mozlinguas_src_unpack"
     "mozlinguas_xpistage_langpacks"
     ;; multibuild
     "multibuild_copy_sources" "multibuild_for_best_variant"
     "multibuild_foreach_variant" "multibuild_merge_root"
     ;; multilib
     "get_abi_CFLAGS" "get_abi_CHOST" "get_abi_CTARGET" "get_abi_FAKE_TARGETS"
     "get_abi_LDFLAGS" "get_abi_LIBDIR" "get_all_abis" "get_all_libdirs"
     "get_exeext" "get_install_abis" "get_libname" "get_modname"
     "has_multilib_profile" "is_final_abi" "multilib_env"
     "multilib_toolchain_setup" "number_abis"
     ;; multilib-build
     "multilib_check_headers" "multilib_copy_sources" "multilib_foreach_abi"
     "multilib_get_enabled_abi_pairs" "multilib_get_enabled_abis"
     "multilib_install_wrappers" "multilib_is_native_abi"
     "multilib_native_enable" "multilib_native_use"
     "multilib_native_use_enable" "multilib_native_use_with"
     "multilib_native_usev" "multilib_native_usex" "multilib_native_with"
     "multilib_parallel_foreach_abi" "multilib_prepare_wrappers"
     ;; multilib-minimal
     "multilib-minimal_src_compile" "multilib-minimal_src_configure"
     "multilib-minimal_src_install" "multilib-minimal_src_test"
     ;; multiprocessing
     "get_makeopts_jobs" "get_makeopts_loadavg" "get_nproc" "makeopts_jobs"
     "makeopts_loadavg"
     ;; myspell-r2
     "myspell-r2_src_install" "myspell-r2_src_unpack"
     ;; netsurf
     "netsurf_define_makeconf"
     ;; ninja-utils
     "eninja" "get_NINJAOPTS"
     ;; nuget
     "nuget_donuget" "nuget_link" "nuget_link-nuget-archives"
     "nuget_link-system-nugets" "nuget_unpack-non-nuget-archives"
     "nuget_writeconfig"
     ;; office-ext-r1
     "office-ext-r1_src_install" "office-ext-r1_src_unpack"
     ;; opam
     "opam-install" "opam_src_install"
     ;; optfeature
     "optfeature" "optfeature_header"
     ;; out-of-source
     "out-of-source_src_compile" "out-of-source_src_configure"
     "out-of-source_src_install" "out-of-source_src_test"
     ;; out-of-source-utils
     "run_in_build_dir"
     ;; pam
     "cleanpamd" "dopamd" "dopammod" "dopamsecurity" "getpam_mod_dir"
     "newpamd" "newpammod" "newpamsecurity" "pamd_mimic" "pamd_mimic_system"
     "pammod_hide_symbols"
     ;; pax-utils
     "host-is-pax" "list-paxables" "pax-mark"
     ;; perl-functions
     "perl_check_env" "perl_delete_emptybsdir" "perl_delete_localpod"
     "perl_delete_module_manpages" "perl_delete_packlist" "perl_doexamples"
     "perl_domodule" "perl_fix_osx_extra" "perl_fix_packlist"
     "perl_fix_permissions" "perl_get_module_version" "perl_get_raw_vendorlib"
     "perl_get_vendorlib" "perl_get_wikiurl" "perl_get_wikiurl_features"
     "perl_get_wikiurl_tests" "perl_has_module" "perl_has_module_version"
     "perl_link_duallife_scripts" "perl_remove_temppath" "perl_rm_files"
     "perl_set_version"
     ;; perl-module
     "perl-module_pkg_postinst" "perl-module_pkg_postrm"
     "perl-module_src_compile" "perl-module_src_configure"
     "perl-module_src_install" "perl-module_src_prepare"
     "perl-module_src_test"
     ;; php-ext-pecl-r3
     "php-ext-pecl-r3_src_install" "php-ext-pecl-r3_src_test"
     ;; php-ext-source-r3
     "php-ext-source-r3_addtoinifiles" "php-ext-source-r3_createinifiles"
     "php-ext-source-r3_phpize" "php-ext-source-r3_src_compile"
     "php-ext-source-r3_src_configure" "php-ext-source-r3_src_install"
     "php-ext-source-r3_src_prepare" "php-ext-source-r3_src_test"
     "php_get_slots" "php_init_slot_env"
     ;; php-pear-r2
     "php-pear-r2_install_packagexml" "php-pear-r2_pkg_postinst"
     "php-pear-r2_pkg_postrm" "php-pear-r2_src_install"
     ;; plocale
     "plocale_find_changes" "plocale_for_each_disabled_locale"
     "plocale_for_each_locale" "plocale_get_locales"
     ;; portability
     "dlopen_lib" "get_bmake" "get_mounts" "seq" "treecopy"
     ;; postgres
     "postgres_check_slot" "postgres_pkg_setup"
     ;; postgres-multi
     "postgres-multi_forbest" "postgres-multi_foreach"
     "postgres-multi_pkg_setup" "postgres-multi_src_compile"
     "postgres-multi_src_install" "postgres-multi_src_prepare"
     "postgres-multi_src_test"
     ;; prefix
     "eprefixify" "hprefixify" "prefixify_ro"
     ;; preserve-libs
     "preserve_old_lib" "preserve_old_lib_notify"
     ;; pypi
     "pypi_normalize_name" "pypi_sdist_url" "pypi_translate_version"
     "pypi_wheel_name" "pypi_wheel_url"
     ;; python-any-r1
     "python-any-r1_pkg_setup" "python_gen_any_dep" "python_setup"
     ;; python-r1
     "python_copy_sources" "python_foreach_impl" "python_gen_any_dep"
     "python_gen_cond_dep" "python_gen_impl_dep" "python_gen_useflags"
     "python_replicate_script" "python_setup"
     ;; python-single-r1
     "python-single-r1_pkg_setup" "python_gen_cond_dep" "python_gen_impl_dep"
     "python_gen_useflags" "python_setup"
     ;; python-utils-r1
     "build_sphinx" "epytest" "eunittest" "python_doexe" "python_doheader"
     "python_domodule" "python_doscript" "python_export_utf8_locale"
     "python_fix_shebang" "python_get_CFLAGS" "python_get_LIBS"
     "python_get_PYTHON_CONFIG" "python_get_includedir"
     "python_get_library_path" "python_get_scriptdir" "python_get_sitedir"
     "python_get_stdlib" "python_has_version" "python_moduleinto"
     "python_newexe" "python_newscript" "python_optimize" "python_scriptinto"
     ;; qmail
     "dospp" "dosupervise" "genqmail_src_unpack" "is_prime"
     "qmail_base_install" "qmail_config_fast" "qmail_config_install"
     "qmail_config_notice" "qmail_maildir_install" "qmail_man_install"
     "qmail_queue_setup" "qmail_rootmail_fixup" "qmail_sendmail_install"
     "qmail_set_cc" "qmail_spp_install" "qmail_spp_src_compile"
     "qmail_spp_src_unpack" "qmail_src_compile" "qmail_src_install"
     "qmail_src_postunpack" "qmail_ssl_generate" "qmail_ssl_install"
     "qmail_supervise_config_notice" "qmail_supervise_install"
     "qmail_supervise_install_one" "qmail_tcprules_build"
     "qmail_tcprules_config" "qmail_tcprules_install"
     ;; qmake-utils
     "eqmake5" "eqmake6" "qt5_get_bindir" "qt5_get_headerdir" "qt5_get_libdir"
     "qt5_get_mkspecsdir" "qt5_get_plugindir" "qt5_get_qmake_args"
     "qt6_get_bindir" "qt6_get_headerdir" "qt6_get_libdir"
     "qt6_get_mkspecsdir" "qt6_get_plugindir" "qt6_get_qmake_args"
     ;; qt5-build
     "qt5-build_pkg_postinst" "qt5-build_pkg_postrm" "qt5-build_src_compile"
     "qt5-build_src_configure" "qt5-build_src_install" "qt5-build_src_prepare"
     "qt5-build_src_test" "qt5_configure_oos_quirk"
     "qt5_symlink_binary_to_path" "qt5_syncqt_version" "qt_use"
     "qt_use_compile_test" "qt_use_disable_config" "qt_use_disable_mod"
     ;; qt6-build
     "qt6-build_src_configure" "qt6-build_src_install" "qt6-build_src_prepare"
     "qt6-build_src_test" "qt6-build_src_unpack" "qt_feature"
     ;; readme.gentoo-r1
     "readme.gentoo_create_doc" "readme.gentoo_print_elog"
     ;; rebar
     "erebar" "rebar_src_compile" "rebar_src_configure" "rebar_src_install"
     "rebar_src_prepare" "rebar_src_test"
     ;; rebar-utils
     "get_erl_libs" "rebar_disable_coverage" "rebar_fix_include_path"
     "rebar_remove_deps" "rebar_set_vsn"
     ;; rebar3
     "erebar3" "rebar3_install_lib" "rebar3_src_compile"
     "rebar3_src_configure" "rebar3_src_install" "rebar3_src_prepare"
     "rebar3_src_test"
     ;; rocm
     "check_amdgpu" "get_amdgpu_flags"
     ;; rpm
     "rpm_src_unpack" "rpm_unpack" "srcrpm_unpack"
     ;; ruby-fakegem
     "all_fakegem_compile" "all_fakegem_install" "all_ruby_compile"
     "all_ruby_install" "all_ruby_unpack" "each_fakegem_compile"
     "each_fakegem_configure" "each_fakegem_install" "each_fakegem_test"
     "each_ruby_compile" "each_ruby_configure" "each_ruby_install"
     "each_ruby_test" "ruby_fakegem_binwrapper" "ruby_fakegem_doins"
     "ruby_fakegem_extensions_installed" "ruby_fakegem_extensionsdir"
     "ruby_fakegem_gemsdir" "ruby_fakegem_gemspec_gemspec"
     "ruby_fakegem_genspec" "ruby_fakegem_install_gemspec"
     "ruby_fakegem_metadata_gemspec" "ruby_fakegem_newins"
     ;; ruby-ng
     "doruby" "ruby-ng_cucumber" "ruby-ng_pkg_setup" "ruby-ng_rspec"
     "ruby-ng_src_compile" "ruby-ng_src_configure" "ruby-ng_src_install"
     "ruby-ng_src_prepare" "ruby-ng_src_test" "ruby-ng_src_unpack"
     "ruby-ng_sus" "ruby-ng_testrb-2" "ruby_add_bdepend" "ruby_add_depend"
     "ruby_add_rdepend" "ruby_get_hdrdir" "ruby_get_implementation"
     "ruby_get_libruby" "ruby_get_use_implementations" "ruby_get_use_targets"
     "ruby_get_version" "ruby_implementation_command"
     "ruby_implementation_depend" "ruby_implementations_depend"
     "ruby_rbconfig_value" "ruby_samelib"
     ;; ruby-ng-gnome2
     "all_ruby_install" "all_ruby_prepare" "each_ruby_compile"
     "each_ruby_configure" "each_ruby_install" "each_ruby_test"
     "ruby-ng-gnome2_all_ruby_prepare"
     ;; rust-toolchain
     "rust_abi" "rust_all_arch_uris" "rust_arch_uri"
     ;; s6
     "s6_get_servicedir" "s6_install_service" "s6_service_down"
     "s6_service_nosetsid"
     ;; savedconfig
     "restore_config" "save_config" "savedconfig_pkg_postinst"
     ;; scons-utils
     "escons"
     ;; secureboot
     "secureboot_auto_sign" "secureboot_pkg_setup" "secureboot_sign_efi_file"
     ;; selinux-policy-2
     "selinux-policy-2_pkg_postinst" "selinux-policy-2_pkg_postrm"
     "selinux-policy-2_src_compile" "selinux-policy-2_src_install"
     "selinux-policy-2_src_prepare" "selinux-policy-2_src_unpack"
     ;; sgml-catalog-r1
     "sgml-catalog-r1_pkg_postinst" "sgml-catalog-r1_pkg_postrm"
     "sgml-catalog-r1_update_catalog" "sgml-catalog-r1_update_env"
     ;; shell-completion
     "dofishcomp" "dozshcomp" "get_fishcompdir" "get_zshcompdir" "newfishcomp"
     "newzshcomp"
     ;; ssl-cert
     "install_cert"
     ;; stardict
     "stardict_src_compile" "stardict_src_install"
     ;; strip-linguas
     "strip-linguas"
     ;; subversion
     "subversion_fetch" "subversion_pkg_preinst" "subversion_src_unpack"
     "subversion_wc_info"
     ;; systemd
     "systemd_dounit" "systemd_douserunit" "systemd_enable_ntpunit"
     "systemd_enable_service" "systemd_get_sleepdir"
     "systemd_get_systemgeneratordir" "systemd_get_systempresetdir"
     "systemd_get_systemunitdir" "systemd_get_userunitdir"
     "systemd_get_utildir" "systemd_install_dropin" "systemd_install_serviced"
     "systemd_is_booted" "systemd_newunit" "systemd_newuserunit"
     "systemd_reenable" "systemd_update_catalog"
     ;; texlive-common
     "dobin_texmf_scripts" "efmtutil-sys" "etexlinks" "etexmf-update"
     "texlive-common_append_to_src_uri" "texlive-common_do_symlinks"
     "texlive-common_handle_config_files"
     "texlive-common_is_file_present_in_texmf" "texlive-common_update_tlpdb"
     ;; texlive-module
     "texlive-module_add_format" "texlive-module_make_language_dat_lines"
     "texlive-module_make_language_def_lines"
     "texlive-module_make_language_lua_lines" "texlive-module_pkg_postinst"
     "texlive-module_pkg_postrm" "texlive-module_src_compile"
     "texlive-module_src_install" "texlive-module_src_unpack"
     "texlive-module_synonyms_to_language_lua_line"
     ;; tmpfiles
     "dotmpfiles" "newtmpfiles" "tmpfiles_process"
     ;; toolchain
     "XGCC" "create_gcc_env_entry" "create_revdep_rebuild_entry"
     "do_gcc_config" "do_gcc_gentoo_patches" "downgrade_arch_flags"
     "fix_libtool_libdir_paths" "gcc-abi-map" "gcc-lang-supported"
     "gcc-multilib-configure" "gcc_do_filter_flags" "gcc_do_make"
     "gcc_movelibs" "gentoo_urls" "get_gcc_src_uri" "get_make_var" "is_ada"
     "is_crosscompile" "is_cxx" "is_d" "is_f77" "is_f95" "is_fortran" "is_go"
     "is_jit" "is_modula2" "is_multilib" "is_objc" "is_objcxx" "is_rust"
     "setup_multilib_osdirnames" "should_we_gcc_config"
     "tc_enable_hardened_gcc" "tc_has_feature" "tc_is_live"
     "tc_use_major_version_only" "tc_version_is_at_least"
     "tc_version_is_between" "toolchain_death_notice" "toolchain_pkg_postinst"
     "toolchain_pkg_postrm" "toolchain_pkg_preinst" "toolchain_pkg_pretend"
     "toolchain_pkg_setup" "toolchain_src_compile" "toolchain_src_configure"
     "toolchain_src_install" "toolchain_src_prepare" "toolchain_src_test"
     "toolchain_src_unpack"
     ;; toolchain-autoconf
     "slot_info_pages" "toolchain-autoconf_src_configure"
     "toolchain-autoconf_src_install" "toolchain-autoconf_src_prepare"
     ;; toolchain-funcs
     "clang-fullversion" "clang-major-version" "clang-micro-version"
     "clang-minor-version" "clang-version" "econf_build" "gcc-fullversion"
     "gcc-major-version" "gcc-micro-version" "gcc-minor-version"
     "gcc-specs-directive" "gcc-specs-nostrict" "gcc-specs-now"
     "gcc-specs-pie" "gcc-specs-relro" "gcc-specs-ssp" "gcc-specs-ssp-to-all"
     "gcc-specs-stack-check" "gcc-version" "gen_usr_ldscript" "tc-arch"
     "tc-arch-kernel" "tc-check-openmp" "tc-cpp-is-true"
     "tc-detect-is-softfloat" "tc-enables-cxx-assertions"
     "tc-enables-fortify-source" "tc-enables-pie" "tc-enables-ssp"
     "tc-enables-ssp-all" "tc-enables-ssp-strong" "tc-endian" "tc-env_build"
     "tc-export" "tc-export_build_env" "tc-get-build-ptr-size"
     "tc-get-c-rtlib" "tc-get-compiler-type" "tc-get-cxx-stdlib"
     "tc-get-ptr-size" "tc-getAR" "tc-getAS" "tc-getBUILD_AR" "tc-getBUILD_AS"
     "tc-getBUILD_CC" "tc-getBUILD_CPP" "tc-getBUILD_CXX" "tc-getBUILD_LD"
     "tc-getBUILD_NM" "tc-getBUILD_OBJCOPY" "tc-getBUILD_PKG_CONFIG"
     "tc-getBUILD_PROG" "tc-getBUILD_RANLIB" "tc-getBUILD_READELF"
     "tc-getBUILD_STRINGS" "tc-getBUILD_STRIP" "tc-getCC" "tc-getCPP"
     "tc-getCXX" "tc-getDLLWRAP" "tc-getF77" "tc-getFC" "tc-getGCJ" "tc-getGO"
     "tc-getLD" "tc-getNM" "tc-getOBJCOPY" "tc-getOBJDUMP" "tc-getPKG_CONFIG"
     "tc-getPROG" "tc-getRANLIB" "tc-getRC" "tc-getREADELF" "tc-getSTRINGS"
     "tc-getSTRIP" "tc-getTARGET_CPP" "tc-has-64bit-time_t" "tc-has-tls"
     "tc-is-clang" "tc-is-cross-compiler" "tc-is-gcc" "tc-is-lto"
     "tc-is-softfloat" "tc-is-static-only" "tc-ld-disable-gold"
     "tc-ld-force-bfd" "tc-ld-is-bfd" "tc-ld-is-gold" "tc-ld-is-lld"
     "tc-ld-is-mold" "tc-ninja_magic_to_arch" "tc-stack-grows-down"
     "tc-tuple-is-softfloat"
     ;; tree-sitter-grammar
     "tree-sitter-grammar_src_compile" "tree-sitter-grammar_src_configure"
     "tree-sitter-grammar_src_install" "tree-sitter-grammar_src_prepare"
     "tree-sitter-grammar_src_test"
     ;; udev
     "get_udevdir" "udev_dorules" "udev_get_udevdir" "udev_newrules"
     "udev_reload"
     ;; unpacker
     "find_unpackable_file" "unpack_7z" "unpack_banner" "unpack_cpio"
     "unpack_deb" "unpack_gpkg" "unpack_lha" "unpack_makeself" "unpack_pdv"
     "unpack_rar" "unpack_zip" "unpacker" "unpacker_src_unpack"
     "unpacker_src_uri_depends"
     ;; user-info
     "egetcomment" "egetent" "egetgroupname" "egetgroups" "egethome"
     "egetshell" "egetusername"
     ;; usr-ldscript
     "gen_usr_ldscript"
     ;; vala
     "vala_api_versions" "vala_best_api_version" "vala_depend" "vala_setup"
     ;; vcs-clean
     "ecvs_clean" "egit_clean" "esvn_clean"
     ;; vcs-snapshot
     "vcs-snapshot_src_unpack"
     ;; vdr-plugin-2
     "fix_vdr_libsi_include" "vdr-plugin-2_pkg_config"
     "vdr-plugin-2_pkg_postinst" "vdr-plugin-2_pkg_postrm"
     "vdr-plugin-2_pkg_setup" "vdr-plugin-2_src_compile"
     "vdr-plugin-2_src_install" "vdr-plugin-2_src_prepare"
     "vdr-plugin-2_src_unpack" "vdr-plugin-2_src_util"
     "vdr_remove_i18n_include"
     ;; verify-sig
     "verify-sig_src_unpack" "verify-sig_verify_detached"
     "verify-sig_verify_message" "verify-sig_verify_signed_checksums"
     "verify-sig_verify_unsigned_checksums"
     ;; vim-doc
     "update_vim_helptags"
     ;; vim-plugin
     "display_vim_plugin_help" "update_vim_afterscripts"
     "vim-plugin_pkg_postinst" "vim-plugin_pkg_postrm"
     "vim-plugin_src_install" "vim-plugin_src_prepare"
     ;; vim-spell
     "vim-spell_pkg_postinst" "vim-spell_src_install"
     ;; virtualx
     "virtx"
     ;; waf-utils
     "waf-utils_src_compile" "waf-utils_src_configure" "waf-utils_src_install"
     ;; webapp
     "need_httpd" "need_httpd_cgi" "need_httpd_fastcgi"
     "webapp_check_installedat" "webapp_checkfileexists" "webapp_configfile"
     "webapp_getinstalltype" "webapp_hook_script" "webapp_pkg_postinst"
     "webapp_pkg_prerm" "webapp_pkg_setup" "webapp_postinst_txt"
     "webapp_postupgrade_txt" "webapp_read_config" "webapp_server_configfile"
     "webapp_serverowned" "webapp_sqlscript" "webapp_src_install"
     "webapp_src_preinst"
     ;; wrapper
     "make_wrapper"
     ;; wxwidgets
     "setup-wxwidgets"
     ;; xdg
     "xdg_pkg_postinst" "xdg_pkg_postrm" "xdg_pkg_preinst" "xdg_src_prepare"
     ;; xdg-utils
     "xdg_desktop_database_update" "xdg_environment_reset"
     "xdg_icon_cache_update" "xdg_mimeinfo_database_update"
     ;; xemacs-packages
     "xemacs-packages_src_install" "xemacs-packages_src_unpack"
     ;; xorg-3
     "create_fonts_dir" "create_fonts_scale" "multilib_src_compile"
     "multilib_src_configure" "multilib_src_install" "remove_font_metadata"
     "xorg-3_flags_setup" "xorg-3_font_configure" "xorg-3_pkg_postinst"
     "xorg-3_pkg_postrm" "xorg-3_pkg_setup" "xorg-3_reconf_source"
     "xorg-3_src_compile" "xorg-3_src_configure" "xorg-3_src_install"
     "xorg-3_src_prepare" "xorg-3_src_unpack"
     ;; @@KEYWORDS-END@@
     )
    font-lock-type-face))

(provide 'ebuild-mode-keywords)

;; Local Variables:
;; coding: utf-8
;; fill-column: 78
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; ebuild-mode-keywords.el ends here
