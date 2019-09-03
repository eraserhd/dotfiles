{ config, pkgs, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        jdk = super.jdk11;
      })
    ];

    environment.systemPackages = with pkgs; [
      clojure
      leiningen
      parinfer-rust
    ];

    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".clojure/deps.edn".text = ''
;; The deps.edn file describes the information needed to build a classpath.
;;
;; When using the `clojure` or `clj` script, there are several deps.edn files
;; that are combined:
;; - install-level
;; - user level (this file)
;; - project level (current directory when invoked)
;;
;; For all attributes other than :paths, these config files are merged left to right.
;; Only the last :paths is kept and others are dropped.
{:aliases
 {:zprint {:extra-deps {org.clojure/clojure {:mvn/version "1.9.0"}
                        zprint              {:mvn/version "0.4.13"}}
           :main-opts ["-m" "zprint.main"]}}}
  ;; Paths
  ;;   Directories in the current project to include in the classpath

  ;; :paths ["src"]

  ;; External dependencies
 
  ;; :deps {
  ;;   org.clojure/clojure {:mvn/version "1.9.0"}
  ;; }

  ;; Aliases
;;   resolve-deps aliases (-R) affect dependency resolution, options:
;;     :extra-deps - specifies extra deps to add to :deps
;;     :override-deps - specifies a coordinate to use instead of that in :deps
;;     :default-deps - specifies a coordinate to use for a lib if one isn't found
;;   make-classpath aliases (-C) affect the classpath generation, options:
;;     :extra-paths - vector of additional paths to add to the classpath
;;     :classpath-overrides - map of lib to path that overrides the result of resolving deps

  ;; :aliases {
  ;;   :deps {:extra-deps {org.clojure/tools.deps.alpha {:mvn/version "0.5.460"}}}
  ;;   :test {:extra-paths ["test"]}
  ;; }

  ;; Provider attributes

  ;; :mvn/repos {
  ;;   "central" {:url "https://repo1.maven.org/maven2/"}
  ;;   "clojars" {:url "https://repo.clojars.org/"}
  ;; }
      '';
    };
  };
}
