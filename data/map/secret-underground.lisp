(in-package :yadfa/zones)
(defzone (0 0 0 secret-underground) ()
    (:default-initargs
        :name "Secret Underground"
        :description "You see several warp pipes in here going to various places"
        :enter-text "You're wandering around in the secret underground"
        :warp-points '(home (0 1 0 home)
                          downtown (0 0 0 downtown)
                          bandits-domain (-3 21 0 bandits-domain))))
