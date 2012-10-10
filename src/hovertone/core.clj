(ns hovertone.core
  ( :use overtone.live overtone.inst.synth))

(definst kick [freq 120 dur 0.3 width 0.5 vol 1]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (* vol  (compander drum drum 0.2 1 0.1 0.01 0.01))))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst trem [freq 440 depth 10 rate 6 length 3]
    (* 0.15
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))


(definst triangle-wave [freq 440 attack 0.01 sustain 0.1
                        release 0.4 vol 0.4] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))

(definst mew [vol 1.0]
  (* vol (+ (* 0.2 (line:kr 1.3 0.3 0.3 FREE)
               (env-gen (perc 0.05 0.02 0.2))
               (lf-tri 600))
            (* 0.4
               (line:kr 0 1 0.5 FREE)
               (sin-osc [440 444])
               (sin-osc:kr 5.7)
               (env-gen (perc 0.1 0.1)))
            (* (line:kr 0.05 1 0.9 FREE)
               (sin-osc [220 224])
               (env-gen (perc 0.1 0.05))))))

(definst foo [freq 220 attack 0.01 sustain 0.1
                        release 0.2 vol 0.1] 
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (bpf (+ (* 0.5 (pink-noise freq)) (sin-osc freq)) 1000 3.5)
     vol))

(defsynth dubstep [bpm 120 wobble 1 note 50 snare-vol 1 kick-vol 1 v 1 dur 4]
 (let [trig (impulse:kr (/ bpm 120))
       freq (midicps note)
       swr (demand trig 0 (dseq [wobble] INF))
       sweep (lin-exp (lf-tri swr) -1 1 40 3000)
       wob (apply + (saw (* freq [0.99 1.01])))
       wob (lpf wob sweep)
       wob (* 0.8 (normalizer wob))
       wob (+ wob (bpf wob 1500 2))
       wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))
       kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
       kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       kick (clip2 kick 1)
       snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
       snare (+ snare (bpf (* 4 snare) 2000))
       snare (clip2 snare 1)]
   (out 0 (* v (clip2 (+ wob (* kick-vol kick) (* snare-vol snare)) 1)
             (line:kr 0 dur dur FREE)))))
(definst spooky-house [freq 440 width 0.2 
                         attack 0.3 sustain 4 release 0.3 
                         vol 0.4] 
  (* 0.3 (env-gen (lin-env attack sustain release) 0.5 0.5 0 0.5 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.25 0 width))))
     vol))

(def metro (metronome 120))
(declare beat1)
(declare beat2)

(defn beat1 [beat]
  (at (metro beat) (#'kick))
  (if (= 0 (mod beat 2))
    (at (metro (+ 0.17 beat)) (#'kick)))
  (if (= 0 (mod beat 12))
    (at (metro  beat) (#'spooky-house)))
  (if (= 0 (mod beat 6))
    (at (metro  beat) (#'spooky-house 220 0.2 0.3 3 0.3 0.8)))
  (if (= 0 (mod beat 8))
    (at (metro  beat) (dubstep 120 0.5 50 1 1 0.03 3.5)))
  (at (metro (+ 0.35 beat)) (c-hat))
  (if (even? beat) (at (metro (+ 0.45 beat)) (c-hat)))
  (at (metro (+ 0.5 beat)) (#'mew 1))
  (apply-at (metro (inc beat))
            (if (= 0 (mod beat 200)) #'beat2 #'beat1)
            (inc beat) []))

(defn beat2 [beat]
  (at (metro beat) (#'kick 220 0.3 0.5 1.5))
  (at (+ 1.1 (metro beat)) (* 0.3  (#'kick 220 0.3 0.5 1.5)))
  (at (+ 0.5  (metro beat)) (#'kick 320 0.4 0.5 1.5))
  (if (= 0 (mod beat 2))
    (at (metro (+ 0.17 beat)) (#'kick)))
  (at (metro (+ 0.35 beat)) (c-hat))
  (if (even? beat) (at (metro (+ 0.45 beat)) (c-hat)))
  (at (metro (+ 0.5 beat)) (#'mew 2))
  (apply-at (metro (inc beat))
            (if (= 0 (mod beat 100)) #'beat3 #'beat2)
            (inc beat) []))

(defn beat3 [beat]
  (if (= 0 (mod beat 2))
    (at (metro (+ 0.17 beat)) (#'kick)))
  (at (metro (+ 0.35 beat)) (c-hat))
  (at (metro (+ 0.65 beat)) (c-hat))
  (if (even? beat) (at (metro (+ 0.45 beat)) (c-hat)))
  (at (metro (+ 0.5 beat)) (#'mew 2))
  (apply-at (metro (inc beat))
            (if (= 0 (mod beat 200)) #'beat1 #'beat3)
            (inc beat) []))

