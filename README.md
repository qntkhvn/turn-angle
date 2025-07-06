# turn-angle

## Abstract

Change of direction is a key element of player movement in American football, yet there remains a lack of objective approaches for in-game performance evaluation of this athletic trait.
Using tracking data, we propose a Bayesian mixed-effects model with heterogeneous variances for assessing a player's ability to make variable directional adjustments while moving on the field.
We model the turn angle (i.e., angle between successive displacement vectors) for NFL ball carriers on both passing and rushing plays, focusing on receivers after the catch and running backs after the handoff.
In particular, we consider a von Mises distribution for the frame-level turn angle and explicitly model both the mean and concentration parameters with relevant spatiotemporal and contextual covariates.
Of primary interest, we include player random effects that allow the turn angle concentration to vary by ball carrier nested within position groups.
This offers practical insight into player evaluation, as it reveals the shiftiest ball carriers with great variability in turning behavior.
We illustrate our approach with results from the first nine weeks of the 2022 NFL regular season and explore player-specific and positional differences in turn angle variability.

## Related links

* [arXiv preprint](https://arxiv.org/abs/XXXXXX)

* Download NFL Big Data Bowl 2025 data from Kaggle [here](https://www.kaggle.com/competitions/nfl-big-data-bowl-2025/data) and store the `.csv` files in a `data` subdirectory.
