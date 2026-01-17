;;; ebuild-mode-keywords.el --- keywords for font-lock  -*-lexical-binding:t-*-

;; Copyright 2006-2026 Gentoo Authors

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

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
  '(("best_version" "debug-print" "debug-print-function" "debug-print-section"
     "die" "diropts" "dobin" "docinto" "doconfd" "dodir" "dodoc" "doenvd"
     "doexe" "doinfo" "doinitd" "doins" "dolib.a" "dolib.so" "doman" "dosbin"
     "dosym" "ebegin" "econf" "eend" "eerror" "einfo" "einfon" "elog" "emake"
     "ewarn" "exeinto" "exeopts" "EXPORT_FUNCTIONS" "fowners" "fperms" "has"
     "has_version" "inherit" "insinto" "insopts" "into" "keepdir" "newbin"
     "newconfd" "newdoc" "newenvd" "newexe" "newinitd" "newins" "newlib.a"
     "newlib.so" "newman" "newsbin" "unpack" "use" "usev" "use_enable"
     "use_with"
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
     "dostrip" "eqawarn" "ver_cut" "ver_rs" "ver_test"
     ;; EAPI 9
     "edo" "pipestatus" "ver_replacing")
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
  '(("assert" "dohard" "dohtml" "dolib" "domo" "dosed" "einstall" "hasq"
     "hasv" "libopts" "portageq" "prepall" "prepalldocs" "prepallinfo"
     "prepallman" "prepallstrip" "prepinfo" "preplib" "prepman" "prepstrip"
     "useq")
    font-lock-warning-face))

(defvar ebuild-mode-keywords-warn
  ;; warn about "which" usage, see <200703121910.26067.vapier@gentoo.org>
  ;; https://public-inbox.gentoo.org/gentoo-dev/200703121910.26067.vapier@gentoo.org/
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
     "acct-group_pkg_pretend" "acct-group_src_install"
     "acct-group_pkg_preinst"
     ;; acct-user
     "acct-user_add_deps" "acct-user_pkg_pretend" "acct-user_src_install"
     "acct-user_pkg_preinst" "acct-user_pkg_postinst" "acct-user_pkg_prerm"
     ;; ada
     "ada_export" "ada_wrapper_setup" "ada_setup" "ada_pkg_setup"
     ;; alternatives
     "alternatives_auto_makesym" "alternatives_makesym"
     "alternatives_pkg_postinst" "alternatives_pkg_postrm"
     ;; apache-2
     "setup_mpm" "check_module_critical" "setup_modules"
     "generate_load_module" "apache-2_pkg_setup" "apache-2_src_prepare"
     "apache-2_src_configure" "apache-2_src_install" "apache-2_pkg_postinst"
     ;; apache-module
     "apache-module_src_compile" "apache-module_src_install"
     "apache-module_pkg_postinst"
     ;; app-alternatives
     "get_alternative"
     ;; aspell-dict-r1
     "aspell-dict-r1_src_configure" "aspell-dict-r1_src_install"
     ;; autotools
     "eautoreconf" "eaclocal_amflags" "eaclocal" "eautoheader" "eautoconf"
     "eautomake" "eautopoint" "config_rpath_update"
     ;; bash-completion-r1
     "get_bashcompdir" "dobashcomp" "newbashcomp" "bashcomp_alias"
     ;; cargo
     "cargo_crate_uris" "cargo_gen_config" "cargo_target_dir"
     "cargo_update_crates" "cargo_src_unpack" "cargo_live_src_unpack"
     "cargo_src_configure" "cargo_env" "cargo_src_compile" "cargo_src_install"
     "cargo_src_test"
     ;; cdrom
     "cdrom_get_cds" "cdrom_load_next_cd"
     ;; check-reqs
     "check-reqs_pkg_setup" "check-reqs_pkg_pretend"
     ;; chromium-2
     "chromium_suid_sandbox_check_kernel_config"
     "chromium_remove_language_paks" "chromium_pkg_die"
     ;; cmake
     "cmake_run_in" "cmake_comment_add_subdirectory" "cmake_use_find_package"
     "cmake_prepare-per-cmakelists" "cmake_prepare" "cmake_src_prepare"
     "cmake_src_configure" "cmake_src_compile" "cmake_build" "cmake_src_test"
     "cmake_src_install"
     ;; common-lisp-3
     "common-lisp-3_src_compile" "absolute-path-p"
     "common-lisp-install-one-source" "lisp-file-p"
     "common-lisp-get-fpredicate" "common-lisp-install-sources"
     "common-lisp-install-one-asdf" "common-lisp-install-asdf"
     "common-lisp-3_src_install" "common-lisp-find-lisp-impl"
     "common-lisp-export-impl-args"
     ;; cron
     "docrondir" "docron" "docrontab" "cron_pkg_postinst"
     ;; crossdev
     "target_is_not_host" "is_crosspkg"
     ;; cuda
     "cuda_gccdir" "cuda_sanitize" "cuda_add_sandbox" "cuda_toolkit_version"
     "cuda_cudnn_version" "cuda_src_prepare"
     ;; cvs
     "cvs_fetch" "cvs_src_unpack"
     ;; depend.apache
     "depend.apache_pkg_setup" "want_apache" "want_apache2" "want_apache2_2"
     "want_apache2_4" "need_apache" "need_apache2" "need_apache2_2"
     "need_apache2_4" "has_apache" "has_apache_threads"
     "has_apache_threads_in"
     ;; desktop
     "make_desktop_entry" "make_session_desktop" "domenu" "newmenu" "doicon"
     "newicon"
     ;; dist-kernel-utils
     "dist-kernel_get_image_path" "dist-kernel_install_kernel"
     "dist-kernel_reinstall_initramfs" "dist-kernel_PV_to_KV"
     "dist-kernel_get_module_suffix" "dist-kernel_compressed_module_cleanup"
     ;; distutils-r1
     "distutils_enable_sphinx" "distutils_enable_tests" "esetup.py"
     "distutils_write_namespace" "distutils-r1_python_prepare_all"
     "distutils_wheel_install" "distutils_pep517_install"
     "distutils-r1_python_compile" "distutils-r1_python_test"
     "distutils-r1_python_install" "distutils-r1_python_install_all"
     ;; docs
     "initialize_git_repo" "sphinx_compile" "mkdocs_compile" "doxygen_compile"
     "docs_compile"
     ;; dot-a
     "lto-guarantee-fat" "strip-lto-bytecode"
     ;; dotnet
     "dotnet_pkg_setup" "exbuild" "egacinstall" "dotnet_multilib_comply"
     ;; dotnet-pkg
     "dotnet-pkg_force-compat" "dotnet-pkg_pkg_setup" "dotnet-pkg_src_unpack"
     "dotnet-pkg_remove-bad" "dotnet-pkg_src_prepare"
     "dotnet-pkg_foreach-project" "dotnet-pkg_src_configure"
     "dotnet-pkg_src_compile" "dotnet-pkg_src_test" "dotnet-pkg_src_install"
     ;; dotnet-pkg-base
     "dotnet-pkg-base_get-configuration" "dotnet-pkg-base_get-output"
     "dotnet-pkg-base_get-runtime" "dotnet-pkg-base_setup"
     "dotnet-pkg-base_remove-global-json" "edotnet" "efsi"
     "dotnet-pkg-base_info" "dotnet-pkg-base_sln-remove"
     "dotnet-pkg-base_foreach-solution" "dotnet-pkg-base_restore"
     "dotnet-pkg-base_restore-tools" "dotnet-pkg-base_restore_tools"
     "dotnet-pkg-base_build" "dotnet-pkg-base_test" "dotnet-pkg-base_install"
     "dotnet-pkg-base_launcherinto" "dotnet-pkg-base_append-launchervar"
     "dotnet-pkg-base_append_launchervar" "dotnet-pkg-base_dolauncher"
     "dotnet-pkg-base_dolauncher-portable"
     "dotnet-pkg-base_dolauncher_portable"
     ;; dune
     "edune" "dune-release" "dune-compile" "dune-test" "dune-install"
     ;; eapi8-dosym
     "dosym8"
     ;; ecm
     "ecm_punt_kf_module" "ecm_punt_qt_module" "ecm_punt_bogus_dep"
     "ecm_punt_po_install" "ecm_pkg_pretend" "ecm_pkg_setup" "ecm_src_prepare"
     "ecm_src_configure" "ecm_src_compile" "ecm_src_test" "ecm_src_install"
     "ecm_pkg_preinst" "ecm_pkg_postinst" "ecm_pkg_postrm"
     ;; ecm-common
     "ecm-common_inject_heredoc" "ecm-common-check_deps"
     "ecm-common_pkg_setup" "ecm-common_src_prepare"
     "ecm-common_src_configure"
     ;; edo
     "edob"
     ;; edos2unix
     "edos2unix"
     ;; elisp
     "elisp_pkg_setup" "elisp_src_unpack" "elisp_src_prepare"
     "elisp_src_configure" "elisp_src_compile" "elisp_src_test"
     "elisp_src_install" "elisp_pkg_postinst" "elisp_pkg_postrm"
     "elisp_pkg_info"
     ;; elisp-common
     "elisp-emacs-version" "elisp-check-emacs-version" "elisp-compile"
     "elisp-make-autoload-file" "elisp-org-export-to" "elisp-test-buttercup"
     "elisp-test-ert-runner" "elisp-test-ert" "elisp-enable-tests"
     "elisp-test" "elisp-install" "elisp-modules-install"
     "elisp-site-file-install" "elisp-make-site-file" "elisp-site-regen"
     ;; emboss-r3
     "emboss-r3_src_configure" "emboss-r3_src_install"
     ;; estack
     "estack_push" "estack_pop" "evar_push" "evar_push_set" "evar_pop"
     "eshopts_push" "eshopts_pop" "eumask_push" "eumask_pop"
     ;; fcaps
     "fcaps" "fcaps_pkg_postinst"
     ;; ffmpeg-compat
     "ffmpeg_compat_add_flags" "ffmpeg_compat_get_prefix"
     "ffmpeg_compat_setup"
     ;; findlib
     "check_ocamlfind" "findlib_src_preinst" "findlib_src_install"
     ;; fixheadtails
     "ht_fix_file" "ht_fix_all"
     ;; flag-o-matic
     "all-flag-vars" "filter-flags" "filter-lfs-flags" "filter-lto"
     "filter-ldflags" "append-cppflags" "append-cflags" "append-cxxflags"
     "append-fflags" "append-lfs-flags" "append-ldflags" "append-flags"
     "replace-flags" "replace-cpu-flags" "is-flagq" "is-flag" "is-ldflagq"
     "is-ldflag" "filter-mfpmath" "strip-flags" "test-flag-CC" "test-flag-CXX"
     "test-flag-F77" "test-flag-FC" "test-flag-CCLD" "test-flag-HIPCXX"
     "test-flags-CC" "test-flags-CXX" "test-flags-F77" "test-flags-FC"
     "test-flags-CCLD" "test-flags-HIPCXX" "test-flags" "test_version_info"
     "strip-unsupported-flags" "get-flag" "replace-sparc64-flags"
     "append-libs" "raw-ldflags" "no-as-needed" "test-compile"
     "append-atomic-flags"
     ;; font
     "font_wrap_opentype_compat" "font_xfont_config" "font_fontconfig"
     "font_cleanup_dirs" "font_pkg_setup" "font_src_install"
     "font_pkg_postinst" "font_pkg_postrm"
     ;; font-ebdftopcf
     "ebdftopcf" "font-ebdftopcf_src_compile"
     ;; fortran-2
     "fortran_int64_abi_fflags" "fortran-2_pkg_setup"
     ;; freedict
     "freedict_src_install"
     ;; gap-pkg
     "gap-pkg_dir" "gap-pkg_econf" "gap-pkg_src_configure"
     "gap-pkg_src_compile" "gap-pkg_enable_tests" "gap-pkg_src_test"
     "gap-pkg_src_install"
     ;; ghc-package
     "ghc-getghc" "ghc-getghcpkg" "ghc-getghcpkgbin" "ghc-version"
     "ghc-pm-version" "ghc-cabal-version" "ghc-is-dynamic"
     "ghc-supports-shared-libraries" "ghc-supports-threaded-runtime"
     "ghc-supports-smp" "ghc-supports-interpreter"
     "ghc-supports-parallel-make" "ghc-extract-pm-version" "ghc-libdir"
     "ghc-bindir" "ghc-make-args" "ghc-confdir" "ghc-package-db"
     "ghc-localpkgconfd" "ghc-package-exists" "check-for-collisions"
     "ghc-install-pkg" "ghc-recache-db" "ghc-register-pkg" "ghc-reregister"
     "ghc-unregister-pkg" "ghc-pkgdeps" "ghc-package_pkg_postinst"
     "ghc-package_pkg_prerm" "ghc-package_pkg_postrm"
     ;; git-r3
     "git-r3_fetch" "git-r3_checkout" "git-r3_peek_remote_ref"
     ;; gkrellm-plugin
     "gkrellm-plugin_src_install"
     ;; gnome2
     "gnome2_src_prepare" "gnome2_src_configure" "gnome2_src_compile"
     "gnome2_src_install" "gnome2_pkg_preinst" "gnome2_pkg_postinst"
     "gnome2_pkg_postrm"
     ;; gnome2-utils
     "gnome2_environment_reset" "gnome2_gconf_savelist" "gnome2_gconf_install"
     "gnome2_gconf_uninstall" "gnome2_omf_fix" "gnome2_scrollkeeper_savelist"
     "gnome2_scrollkeeper_update" "gnome2_schemas_savelist"
     "gnome2_schemas_update" "gnome2_gdk_pixbuf_savelist"
     "gnome2_gdk_pixbuf_update" "gnome2_query_immodules_gtk2"
     "gnome2_query_immodules_gtk3" "gnome2_giomodule_cache_update"
     "gnome2_disable_deprecation_warning"
     ;; gnuconfig
     "gnuconfig_update"
     ;; go-env
     "go-env_set_compile_environment" "go-env_goos" "go-env_goarch"
     "go-env_go386" "go-env_goarm"
     ;; go-module
     "ego" "go-module_set_globals" "go-module_setup_proxy"
     "go-module_src_unpack" "go-module_live_vendor"
     ;; golang-base
     "ego_pn_check" "get_golibdir" "get_golibdir_gopath" "golang_install_pkgs"
     ;; golang-vcs-snapshot
     "golang-vcs-snapshot_src_unpack"
     ;; greadme
     "greadme_stdin" "greadme_file" "greadme_pkg_preinst"
     "greadme_pkg_postinst"
     ;; gstreamer-meson
     "gstreamer_system_package" "gstreamer_system_library"
     "gstreamer_multilib_src_configure" "gstreamer_multilib_src_compile"
     "gstreamer-meson_pkg_setup" "gstreamer_multilib_src_test"
     "gstreamer_multilib_src_install" "gstreamer_multilib_src_install_all"
     ;; guile
     "guile_pkg_setup" "guile_copy_sources" "guile_foreach_impl"
     "guile_merge_roots" "guile_for_best_impl" "guile_src_prepare"
     "guile_src_configure" "guile_src_compile" "guile_src_test"
     "guile_src_install"
     ;; guile-single
     "guile_gen_cond_dep" "guile-single_pkg_setup" "guile-single_src_prepare"
     "guile-single_src_install"
     ;; guile-utils
     "guile_check_compat" "guile_set_common_vars"
     "guile_filter_pkgconfig_path" "guile_generate_depstrings"
     "guile_unstrip_ccache" "guile_export" "guile_create_temporary_config"
     "guile_bump_sources"
     ;; haskell-cabal
     "cabal-check-cache" "cabal_flag" "cabal_chdeps" "cabal-constraint"
     "replace-hcflags" "cabal-register-inplace" "cabal-run-dist-bin"
     ;; java-osgi
     "java-osgi_dojar" "java-osgi_newjar" "java-osgi_newjar-fromfile"
     "java-osgi_dojar-fromfile"
     ;; java-pkg-2
     "java-pkg-2_pkg_setup" "java-pkg-2_src_prepare" "java-pkg-2_pkg_preinst"
     ;; java-pkg-opt-2
     "java-pkg-opt-2_pkg_setup" "java-pkg-opt-2_src_prepare"
     "java-pkg-opt-2_pkg_preinst"
     ;; java-pkg-simple
     "java-pkg-simple_src_compile" "java-pkg-simple_src_install"
     "java-pkg-simple_src_test"
     ;; java-utils-2
     "java-pkg_doexamples" "java-pkg_addres" "java-pkg_rm_files"
     "java-pkg_dojar" "java-pkg_regjar" "java-pkg_newjar" "java-pkg_addcp"
     "java-pkg_doso" "java-pkg_regso" "java-pkg_jarinto" "java-pkg_sointo"
     "java-pkg_dohtml" "java-pkg_dojavadoc" "java-pkg_dosrc"
     "java-pkg_dolauncher" "java-pkg_dowar" "java-pkg_jar-from"
     "java-pkg_jarfrom" "java-pkg_getjars" "java-pkg_getjar"
     "java-pkg_register-dependency" "java-pkg_register-optional-dependency"
     "java-pkg_register-environment-variable" "java-pkg_get-bootclasspath"
     "java-pkg_find-normal-jars" "java-pkg_ensure-no-bundled-jars"
     "java-pkg_current-vm-matches" "java-pkg_get-source" "java-pkg_get-target"
     "java-pkg_get-javac" "java-pkg_javac-args" "java-pkg_get-jni-cflags"
     "java-pkg_register-ant-task" "ejunit" "ejunit4"
     "java-utils-2_src_prepare" "java-utils-2_pkg_preinst" "eant" "ejavac"
     "ejavadoc" "java-pkg_filter-compiler" "java-pkg_force-compiler"
     "java-pkg_clean"
     ;; java-vm-2
     "java-vm-2_pkg_setup" "java-vm-2_pkg_postinst" "java-vm-2_pkg_prerm"
     "java-vm-2_pkg_postrm" "get_system_arch" "java-vm_install-env"
     "java-vm_set-pax-markings" "java-vm_revdep-mask"
     "java-vm_sandbox-predict"
     ;; junit5
     "ejunit5"
     ;; kde.org
     "kde.org_pkg_nofetch" "kde.org_src_unpack"
     ;; kernel-2
     "debug-print-kernel2-variables" "handle_genpatches" "detect_version"
     "kernel_is" "kernel_header_destdir" "cross_pre_c_headers"
     "env_setup_kernel_makeopts" "universal_unpack" "unpack_set_extraversion"
     "unpack_fix_install_path" "install_universal" "install_headers"
     "install_sources" "preinst_headers" "postinst_sources" "setup_headers"
     "unipatch" "getfilevar" "detect_arch" "headers___fix"
     "kernel-2_src_unpack" "kernel-2_src_prepare" "kernel-2_src_compile"
     "kernel-2_src_test" "kernel-2_pkg_preinst" "kernel-2_src_install"
     "kernel-2_pkg_postinst" "kernel-2_pkg_setup" "kernel-2_pkg_postrm"
     ;; kernel-build
     "kernel-build_pkg_setup" "kernel-build_src_configure"
     "kernel-build_src_compile" "kernel-build_src_test"
     "kernel-build_src_install" "kernel-build_pkg_postinst"
     "kernel-build_merge_configs"
     ;; kernel-install
     "kernel-install_can_update_symlink" "kernel-install_update_symlink"
     "kernel-install_get_qemu_arch" "kernel-install_create_init"
     "kernel-install_create_qemu_image" "kernel-install_test"
     "kernel-install_pkg_pretend" "kernel-install_src_test"
     "kernel-install_pkg_preinst" "kernel-install_extract_from_uki"
     "kernel-install_install_all" "kernel-install_pkg_postinst"
     "kernel-install_pkg_postrm" "kernel-install_pkg_config"
     "kernel-install_compress_modules"
     ;; latex-package
     "latex-package_src_doinstall" "latex-package_src_compile"
     "latex-package_src_install" "latex-package_pkg_postinst"
     "latex-package_pkg_postrm" "latex-package_rehash"
     ;; libretro-core
     "libretro-core_src_unpack" "libretro-core_src_prepare"
     "libretro-core_src_compile" "libretro-core_src_install"
     ;; libtool
     "elibtoolize"
     ;; linux-info
     "set_arch_to_kernel" "set_arch_to_pkgmgr" "qout" "qeinfo" "qewarn"
     "qeerror" "getfilevar" "getfilevar_noexec" "linux_config_src_exists"
     "linux_config_bin_exists" "linux_config_exists" "linux_config_path"
     "require_configured_kernel" "linux_chkconfig_present"
     "linux_chkconfig_module" "linux_chkconfig_builtin"
     "linux_chkconfig_string" "kernel_is" "get_version" "get_running_version"
     "linux-info_get_any_version" "check_kernel_built"
     "check_modules_supported" "check_extra_config" "check_zlibinflate"
     "linux-info_pkg_setup" "kernel_get_makefile"
     ;; linux-mod
     "use_m" "convert_to_m" "update_moduledb" "remove_moduledb" "set_kvobj"
     "get-KERNEL_CC" "linux-mod_pkg_setup" "linux-mod_pkg_setup_binary"
     "strip_modulenames" "linux-mod_src_compile" "linux-mod_src_install"
     "linux-mod_pkg_preinst" "linux-mod_pkg_postinst" "linux-mod_pkg_postrm"
     ;; linux-mod-r1
     "linux-mod-r1_pkg_setup" "linux-mod-r1_src_compile"
     "linux-mod-r1_src_install" "linux-mod-r1_pkg_postinst" "linux_domodule"
     "linux_moduleinto" "modules_post_process"
     ;; llvm
     "get_llvm_slot" "get_llvm_prefix" "llvm_pkg_setup"
     ;; llvm-r1
     "llvm_gen_dep" "get_llvm_prefix" "llvm-r1_pkg_setup"
     ;; llvm-r2
     "llvm_gen_dep" "get_llvm_prefix" "generate_llvm_config"
     "llvm_cbuild_setup" "llvm_chost_setup" "llvm-r2_pkg_setup"
     ;; llvm-utils
     "llvm_tuple_to_target" "llvm_fix_clang_version" "llvm_fix_tool_path"
     "llvm_prepend_path" "llvm_cmake_use_musl"
     ;; llvm.org
     "llvm.org_set_globals" "llvm.org_src_unpack" "llvm.org_src_prepare"
     "get_lit_flags" "llvm_are_manpages_built" "llvm_install_manpages"
     ;; lua
     "lua_copy_sources" "lua_foreach_impl"
     ;; lua-single
     "lua_gen_cond_dep" "lua_gen_impl_dep" "lua_setup" "lua-single_pkg_setup"
     ;; lua-utils
     "lua_enable_tests" "lua_get_CFLAGS" "lua_get_cmod_dir"
     "lua_get_include_dir" "lua_get_LIBS" "lua_get_lmod_dir"
     "lua_get_shared_lib" "lua_get_version"
     ;; mate
     "mate_py_cond_func_wrap" "ematedocize" "want_mate_doc" "mate_src_prepare"
     "mate_src_configure" "mate_src_install" "mate_pkg_preinst"
     "mate_pkg_postinst" "mate_pkg_postrm"
     ;; mercurial
     "mercurial_fetch" "mercurial_src_unpack"
     ;; meson
     "meson_use" "meson_feature" "setup_meson_src_configure"
     "meson_src_configure" "meson_src_compile" "meson_src_test"
     "meson_install" "meson_src_install"
     ;; meson-multilib
     "meson_native_use_bool" "meson_native_use_feature" "meson_native_enabled"
     "meson_native_true"
     ;; mozcoreconf-v6
     "mozconfig_annotate" "mozconfig_use_enable" "mozconfig_use_with"
     "mozconfig_init" "mozconfig_final"
     ;; mozlinguas-v2
     "mozlinguas_src_unpack" "mozlinguas_mozconfig" "mozlinguas_src_compile"
     "mozlinguas_xpistage_langpacks" "mozlinguas-v2_src_install"
     ;; multibuild
     "multibuild_foreach_variant" "multibuild_for_best_variant"
     "multibuild_copy_sources" "multibuild_merge_root"
     ;; multilib
     "has_multilib_profile" "get_abi_CFLAGS" "get_abi_LDFLAGS" "get_abi_CHOST"
     "get_abi_CTARGET" "get_abi_FAKE_TARGETS" "get_abi_LIBDIR"
     "get_install_abis" "get_all_abis" "get_all_libdirs" "is_final_abi"
     "number_abis" "get_exeext" "get_libname" "get_modname" "multilib_env"
     "multilib_toolchain_setup"
     ;; multilib-build
     "multilib_get_enabled_abis" "multilib_get_enabled_abi_pairs"
     "multilib_foreach_abi" "multilib_parallel_foreach_abi"
     "multilib_check_headers" "multilib_copy_sources"
     "multilib_prepare_wrappers" "multilib_install_wrappers"
     "multilib_is_native_abi" "multilib_native_use" "multilib_native_usev"
     "multilib_native_use_with" "multilib_native_use_enable"
     "multilib_native_enable" "multilib_native_with" "multilib_native_usex"
     ;; multiprocessing
     "get_nproc" "get_makeopts_jobs" "makeopts_jobs" "get_makeopts_loadavg"
     "makeopts_loadavg"
     ;; myspell-r2
     "myspell-r2_src_unpack" "myspell-r2_src_install"
     ;; netsurf
     "netsurf_define_makeconf"
     ;; nginx
     "econf_ngx" "nginx_pkg_setup" "nginx_src_unpack" "nginx_src_prepare"
     "nginx_src_configure" "nginx_src_compile" "nginx_src_test"
     "nginx_src_install" "nginx_pkg_postinst"
     ;; nginx-module
     "econf_ngx" "ngx_mod_pkg_to_sonames" "ngx_mod_append_libs"
     "ngx_mod_setup_link_modules" "ngx_mod_link_module" "ngx_mod_link_lib"
     "nginx-module_src_unpack" "nginx-module_src_prepare"
     "nginx-module_src_configure" "nginx-module_src_compile"
     "nginx-module_src_test" "nginx-module_src_install"
     "nginx-module_pkg_postinst"
     ;; ninja-utils
     "get_NINJAOPTS" "eninja"
     ;; nuget
     "nuget_link" "nuget_link-system-nugets" "nuget_link-nuget-archives"
     "nuget_unpack-non-nuget-archives" "nuget_writeconfig" "nuget_donuget"
     ;; office-ext-r1
     "office-ext-r1_src_unpack" "office-ext-r1_src_install"
     ;; opam
     "opam-install"
     ;; optfeature
     "optfeature_header" "optfeature"
     ;; out-of-source
     "out-of-source_src_configure" "out-of-source_src_compile"
     "out-of-source_src_test" "out-of-source_src_install"
     ;; out-of-source-utils
     "run_in_build_dir"
     ;; pam
     "dopamd" "newpamd" "dopamsecurity" "newpamsecurity" "getpam_mod_dir"
     "pammod_hide_symbols" "dopammod" "newpammod" "pamd_mimic_system"
     "pamd_mimic" "cleanpamd"
     ;; pax-utils
     "pax-mark" "list-paxables" "host-is-pax"
     ;; perl-functions
     "perl_set_version" "perl_delete_localpod" "perl_fix_osx_extra"
     "perl_delete_module_manpages" "perl_delete_packlist"
     "perl_delete_emptybsdir" "perl_fix_permissions" "perl_fix_packlist"
     "perl_remove_temppath" "perl_rm_files" "perl_link_duallife_scripts"
     "perl_check_env" "perl_doexamples" "perl_has_module"
     "perl_has_module_version" "perl_get_module_version"
     "perl_get_raw_vendorlib" "perl_get_vendorlib" "perl_domodule"
     "perl_get_wikiurl"
     ;; perl-module
     "perl-module_src_prepare" "perl-module_src_configure"
     "perl-module_src_compile" "perl-module_src_test"
     "perl-module_src_install" "perl-module_pkg_postinst"
     "perl-module_pkg_postrm"
     ;; php-ext-pecl-r3
     "php-ext-pecl-r3_src_install" "php-ext-pecl-r3_src_test"
     ;; php-ext-source-r3
     "php-ext-source-r3_src_prepare" "php-ext-source-r3_phpize"
     "php-ext-source-r3_src_configure" "php-ext-source-r3_src_compile"
     "php-ext-source-r3_src_install" "php-ext-source-r3_src_test"
     "php_get_slots" "php_init_slot_env" "php-ext-source-r3_createinifiles"
     "php-ext-source-r3_addtoinifiles"
     ;; php-pear-r2
     "php-pear-r2_install_packagexml" "php-pear-r2_src_install"
     "php-pear-r2_pkg_postinst" "php-pear-r2_pkg_postrm"
     ;; plocale
     "plocale_for_each_locale" "plocale_for_each_disabled_locale"
     "plocale_find_changes" "plocale_get_locales"
     ;; portability
     "treecopy" "seq" "dlopen_lib" "get_bmake" "get_mounts"
     ;; postgres
     "postgres_check_slot" "postgres_pkg_setup"
     ;; postgres-multi
     "postgres-multi_foreach" "postgres-multi_forbest"
     "postgres-multi_pkg_setup" "postgres-multi_src_prepare"
     "postgres-multi_src_compile" "postgres-multi_src_test"
     "postgres-multi_src_install"
     ;; prefix
     "eprefixify" "hprefixify" "prefixify_ro"
     ;; preserve-libs
     "preserve_old_lib" "preserve_old_lib_notify"
     ;; pypi
     "pypi_normalize_name" "pypi_translate_version" "pypi_sdist_url"
     "pypi_wheel_name" "pypi_wheel_url" "pypi_provenance_url"
     "pypi_verify_provenance" "pypi_src_unpack"
     ;; python-any-r1
     "python_gen_any_dep" "python_setup" "python-any-r1_pkg_setup"
     ;; python-r1
     "python_gen_useflags" "python_gen_cond_dep" "python_gen_impl_dep"
     "python_gen_any_dep" "python_copy_sources" "python_foreach_impl"
     "python_setup" "python_replicate_script"
     ;; python-single-r1
     "python_gen_useflags" "python_gen_cond_dep" "python_gen_impl_dep"
     "python_setup" "python-single-r1_pkg_setup"
     ;; python-utils-r1
     "python_get_stdlib" "python_get_sitedir" "python_get_includedir"
     "python_get_library_path" "python_get_CFLAGS" "python_get_LIBS"
     "python_get_PYTHON_CONFIG" "python_get_scriptdir" "python_optimize"
     "python_scriptinto" "python_doexe" "python_newexe" "python_doscript"
     "python_newscript" "python_moduleinto" "python_domodule"
     "python_doheader" "python_fix_shebang" "python_export_utf8_locale"
     "build_sphinx" "epytest" "eunittest" "python_has_version"
     ;; qmail
     "is_prime" "dosupervise" "qmail_set_cc" "qmail_src_postunpack"
     ;; qmake-utils
     "qt5_get_bindir" "qt5_get_headerdir" "qt5_get_libdir"
     "qt5_get_mkspecsdir" "qt5_get_plugindir" "qt5_get_qmake_args" "eqmake5"
     "qt6_get_bindir" "qt6_get_headerdir" "qt6_get_libdir"
     "qt6_get_mkspecsdir" "qt6_get_plugindir" "qt6_get_qmake_args" "eqmake6"
     ;; qt5-build
     "qt5-build_src_prepare" "qt5-build_src_configure" "qt5-build_src_compile"
     "qt5-build_src_test" "qt5-build_src_install" "qt5-build_pkg_postinst"
     "qt5-build_pkg_postrm" "qt5_configure_oos_quirk" "qt5_syncqt_version"
     "qt5_symlink_binary_to_path" "qt_use" "qt_use_compile_test"
     "qt_use_disable_config" "qt_use_disable_mod"
     ;; qt6-build
     "qt6-build_src_unpack" "qt6-build_src_prepare" "qt6-build_src_configure"
     "qt6-build_src_test" "qt6-build_src_install" "qt_feature"
     ;; readme.gentoo-r1
     "readme.gentoo_create_doc" "readme.gentoo_print_elog"
     ;; rebar
     "erebar" "rebar_src_prepare" "rebar_src_configure" "rebar_src_compile"
     "rebar_src_test" "rebar_src_install"
     ;; rebar-utils
     "get_erl_libs" "rebar_disable_coverage" "rebar_fix_include_path"
     "rebar_remove_deps" "rebar_set_vsn"
     ;; rebar3
     "erebar3" "rebar3_src_prepare" "rebar3_src_configure"
     "rebar3_src_compile" "rebar3_src_test" "rebar3_install_lib"
     "rebar3_src_install"
     ;; rocm
     "get_amdgpu_flags" "rocm_add_sandbox" "check_amdgpu" "rocm_use_hipcc"
     "rocm_use_clang"
     ;; rpm
     "rpm_unpack" "srcrpm_unpack" "rpm_src_unpack"
     ;; ruby-fakegem
     "ruby_fakegem_gemsdir" "ruby_fakegem_doins" "ruby_fakegem_newins"
     "ruby_fakegem_install_gemspec" "ruby_fakegem_gemspec_gemspec"
     "ruby_fakegem_metadata_gemspec" "ruby_fakegem_genspec"
     "ruby_fakegem_binwrapper" "each_fakegem_configure" "each_ruby_configure"
     "all_fakegem_compile" "each_fakegem_compile" "each_ruby_compile"
     "all_ruby_unpack" "all_ruby_compile" "each_fakegem_test" "each_ruby_test"
     "ruby_fakegem_extensions_installed" "ruby_fakegem_extensionsdir"
     "each_fakegem_install" "each_ruby_install" "all_fakegem_install"
     "all_ruby_install"
     ;; ruby-ng
     "ruby_implementation_depend" "ruby_samelib" "ruby_implementation_command"
     "ruby_add_rdepend" "ruby_add_bdepend" "ruby_add_depend"
     "ruby_get_use_implementations" "ruby_get_use_targets"
     "ruby_implementations_depend" "ruby-ng_pkg_setup" "ruby-ng_src_unpack"
     "ruby-ng_src_prepare" "ruby-ng_src_configure" "ruby-ng_src_compile"
     "ruby-ng_src_test" "ruby-ng_src_install" "ruby_rbconfig_value" "doruby"
     "ruby_get_libruby" "ruby_get_hdrdir" "ruby_get_version"
     "ruby_get_implementation" "ruby-ng_rspec" "ruby-ng_cucumber"
     "ruby-ng_sus" "ruby-ng_testrb-2"
     ;; ruby-ng-gnome2
     "each_ruby_configure" "each_ruby_compile" "each_ruby_install"
     "all_ruby_install" "each_ruby_test"
     ;; rust
     "get_rust_path" "get_rust_prefix" "rust_prepend_path" "rust_pkg_setup"
     ;; rust-toolchain
     "rust_abi" "rust_arch_uri" "rust_all_arch_uris"
     ;; s6
     "s6_get_servicedir" "s6_install_service" "s6_service_down"
     "s6_service_nosetsid"
     ;; savedconfig
     "save_config" "restore_config"
     ;; scons-utils
     "escons"
     ;; sec-keys
     "sec-keys_src_compile" "sec-keys_src_install"
     ;; secureboot
     "secureboot_pkg_setup" "secureboot_sign_efi_file" "secureboot_auto_sign"
     ;; selinux-policy-2
     "selinux-policy-2_src_unpack" "selinux-policy-2_src_prepare"
     "selinux-policy-2_src_compile" "selinux-policy-2_src_install"
     "selinux-policy-2_pkg_postinst" "selinux-policy-2_pkg_postrm"
     ;; sgml-catalog-r1
     "sgml-catalog-r1_update_catalog" "sgml-catalog-r1_update_env"
     ;; shell-completion
     "get_fishcompdir" "get_zshcompdir" "dofishcomp" "dozshcomp" "newfishcomp"
     "newzshcomp"
     ;; ssl-cert
     "install_cert"
     ;; strip-linguas
     "strip-linguas"
     ;; subversion
     "subversion_fetch" "subversion_wc_info" "subversion_src_unpack"
     "subversion_pkg_preinst"
     ;; sysroot
     "qemu_arch" "sysroot_make_run_prefixed" "sysroot_run_prefixed"
     ;; systemd
     "systemd_get_systemunitdir" "systemd_get_userunitdir"
     "systemd_get_utildir" "systemd_get_systemgeneratordir"
     "systemd_get_systempresetdir" "systemd_get_sleepdir" "systemd_dounit"
     "systemd_newunit" "systemd_douserunit" "systemd_newuserunit"
     "systemd_install_serviced" "systemd_install_dropin"
     "systemd_enable_service" "systemd_enable_ntpunit"
     "systemd_update_catalog" "systemd_is_booted" "systemd_reenable"
     ;; texlive-common
     "texlive-common_handle_config_files"
     "texlive-common_is_file_present_in_texmf" "texlive-common_do_symlinks"
     "etexlinks" "dobin_texmf_scripts" "etexmf-update" "efmtutil-sys"
     "texlive-common_append_to_src_uri" "texlive-common_update_tlpdb"
     ;; texlive-module
     "texlive-module_src_unpack" "texlive-module_add_format"
     "texlive-module_make_language_def_lines"
     "texlive-module_make_language_dat_lines"
     "texlive-module_synonyms_to_language_lua_line"
     "texlive-module_make_language_lua_lines" "texlive-module_src_compile"
     "texlive-module_src_install" "texlive-module_pkg_postinst"
     "texlive-module_pkg_postrm"
     ;; tmpfiles
     "dotmpfiles" "newtmpfiles" "tmpfiles_process"
     ;; toolchain
     "tc_version_is_at_least" "tc_version_is_between"
     ;; toolchain-funcs
     "tc-getAR" "tc-getAS" "tc-getCC" "tc-getCPP" "tc-getCXX" "tc-getHIPCXX"
     "tc-getLD" "tc-getSTRINGS" "tc-getSTRIP" "tc-getNM" "tc-getRANLIB"
     "tc-getREADELF" "tc-getOBJCOPY" "tc-getOBJDUMP" "tc-getF77" "tc-getFC"
     "tc-getGCJ" "tc-getGO" "tc-getPKG_CONFIG" "tc-getRC" "tc-getDLLWRAP"
     "tc-getBUILD_AR" "tc-getBUILD_AS" "tc-getBUILD_CC" "tc-getBUILD_CPP"
     "tc-getBUILD_CXX" "tc-getBUILD_LD" "tc-getBUILD_STRINGS"
     "tc-getBUILD_STRIP" "tc-getBUILD_NM" "tc-getBUILD_RANLIB"
     "tc-getBUILD_READELF" "tc-getBUILD_OBJCOPY" "tc-getBUILD_PKG_CONFIG"
     "tc-getTARGET_CPP" "tc-export" "tc-is-cross-compiler" "tc-cpp-is-true"
     "tc-detect-is-softfloat" "tc-tuple-is-softfloat" "tc-is-softfloat"
     "tc-is-static-only" "tc-stack-grows-down" "tc-export_build_env"
     "tc-env_build" "econf_build" "tc-ld-is-bfd" "tc-ld-is-gold"
     "tc-ld-is-lld" "tc-ld-is-mold" "tc-ld-disable-gold" "tc-ld-force-bfd"
     "tc-check-min_ver" "tc-check-openmp" "tc-has-tls" "tc-arch-kernel"
     "tc-arch" "tc-endian" "tc-get-compiler-type" "tc-is-gcc" "tc-is-clang"
     "gcc-fullversion" "gcc-version" "gcc-major-version" "gcc-minor-version"
     "gcc-micro-version" "clang-fullversion" "clang-version"
     "clang-major-version" "clang-minor-version" "clang-micro-version"
     "tc-enables-cxx-assertions" "tc-enables-pie" "tc-enables-fortify-source"
     "tc-enables-ssp" "tc-enables-ssp-strong" "tc-enables-ssp-all"
     "tc-get-cxx-stdlib" "tc-get-c-rtlib" "tc-get-ptr-size"
     "tc-get-build-ptr-size" "tc-is-lto" "tc-has-64bit-time_t"
     ;; tree-sitter-grammar
     "tree-sitter-grammar_src_test"
     ;; udev
     "udev_get_udevdir" "get_udevdir" "udev_dorules" "udev_newrules"
     "udev_reload"
     ;; unpacker
     "unpack_pdv" "unpack_makeself" "unpack_deb" "unpack_cpio" "unpack_zip"
     "unpack_7z" "unpack_rar" "unpack_lha" "unpack_gpkg" "unpacker"
     "unpacker_src_unpack" "unpacker_src_uri_depends"
     ;; user-info
     "egetent" "egetusername" "egetgroupname" "egethome" "egetshell"
     "egetcomment" "egetgroups"
     ;; usr-ldscript
     "gen_usr_ldscript"
     ;; vala
     "vala_api_versions" "vala_depend" "vala_best_api_version" "vala_setup"
     "vala_src_prepare"
     ;; vcs-clean
     "ecvs_clean" "esvn_clean" "egit_clean"
     ;; vcs-snapshot
     "vcs-snapshot_src_unpack"
     ;; vdr-plugin-2
     "fix_vdr_libsi_include" "vdr_remove_i18n_include" "vdr-plugin-2_src_util"
     ;; verify-sig
     "verify-sig_verify_detached" "verify-sig_verify_message"
     "verify-sig_verify_unsigned_checksums"
     "verify-sig_verify_signed_checksums"
     "verify-sig_uncompress_verify_unpack" "verify-sig_src_unpack"
     ;; vim-doc
     "update_vim_helptags"
     ;; vim-plugin
     "vim-plugin_src_prepare" "vim-plugin_src_install"
     "vim-plugin_pkg_postinst" "vim-plugin_pkg_postrm"
     "update_vim_afterscripts" "display_vim_plugin_help"
     ;; vim-spell
     "vim-spell_src_install" "vim-spell_pkg_postinst"
     ;; virtualx
     "virtx"
     ;; waf-utils
     "waf-utils_src_configure" "waf-utils_src_compile" "waf-utils_src_install"
     ;; webapp
     "need_httpd" "need_httpd_cgi" "need_httpd_fastcgi" "webapp_configfile"
     "webapp_hook_script" "webapp_postinst_txt" "webapp_postupgrade_txt"
     "webapp_serverowned" "webapp_server_configfile" "webapp_sqlscript"
     "webapp_src_preinst" "webapp_pkg_setup" "webapp_src_install"
     "webapp_pkg_postinst" "webapp_pkg_prerm"
     ;; wine
     "wine_pkg_pretend" "wine_src_prepare" "wine_src_configure"
     "wine_src_compile" "wine_src_install" "wine_pkg_postinst"
     "wine_pkg_postrm"
     ;; wrapper
     "make_wrapper"
     ;; wxwidgets
     "setup-wxwidgets"
     ;; xdg
     "xdg_pkg_preinst" "xdg_pkg_postinst" "xdg_pkg_postrm"
     ;; xdg-utils
     "xdg_environment_reset" "xdg_desktop_database_update"
     "xdg_icon_cache_update" "xdg_mimeinfo_database_update"
     ;; xorg-3
     "xorg-3_pkg_setup" "xorg-3_src_unpack" "xorg-3_reconf_source"
     "xorg-3_src_prepare" "xorg-3_font_configure" "xorg-3_flags_setup"
     "xorg-3_src_configure" "xorg-3_src_compile" "xorg-3_src_install"
     "xorg-3_pkg_postinst" "xorg-3_pkg_postrm" "remove_font_metadata"
     "create_fonts_scale" "create_fonts_dir"
     ;; xorg-meson
     "xorg-meson_src_unpack" "xorg-meson_src_configure"
     "xorg-meson_src_install"
     ;; zig
     "zig_get_jobs" "zig_init_base_args" "zig_pkg_setup" "zig_live_fetch"
     "zig_src_unpack" "zig_src_prepare" "zig_src_configure" "zig_src_compile"
     "zig_src_test" "zig_src_install"
     ;; zig-utils
     "zig-utils_c_env_to_zig_target" "zig-utils_c_env_to_zig_cpu"
     "zig-utils_find_installation" "zig-utils_setup" "ezig"
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
