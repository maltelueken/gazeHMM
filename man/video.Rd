\name{video}
\alias{video}
\docType{data}
\title{
  Eye-tracking Data from Video Stimuli Used in Andersson et al. (2017)
}
\description{
  This data set contains three examples of subjects following objects in videos while their eye movements were recorded. Each recorded sample was    annotated by two human coders and classified by 10 different algorithms (IDT and IDTk are two versions of the same algorithm) into up to six       different eye movement events.
}
\usage{data("video")}
\format{
  A data frame with 10889 observations on the following 19 variables.
  \describe{
    \item{\code{subject}}{A numeric vector indicating the number of the subject.}
    \item{\code{t}}{A numeric vector containing the timestamp.}
    \item{\code{h_pupil}}{A numeric vector with the horizontal pupil dilation.}
    \item{\code{v_pupil}}{A numeric vector with the vertical pupil dilation.}
    \item{\code{x}}{A numeric vector containing the gaze position on the x-dimension.}
    \item{\code{y}}{A numeric vector containing the gaze position on the y-dimension.}
    \item{\code{coderMN}}{A numeric vector indicating the eye movement event annotated by the first human coder.}
    \item{\code{coderRA}}{A numeric vector indicating the eye movement event annotated by the second human coder.}
    \item{\code{CDT}}{A numeric vector indicating the eye movement event classified by the CDT (only fixations) algorithm.}
    \item{\code{EK}}{A numeric vector indicating the eye movement event classified by the EK (only saccades) algorithm.}
    \item{\code{IDT}}{A numeric vector indicating the eye movement event classified by the IDT (only fixations) algorithm.}
    \item{\code{IDTk}}{A numeric vector indicating the eye movement event classified by the IDTk (fixations and saccades) algorithm.}
    \item{\code{IKF}}{A numeric vector indicating the eye movement event classified by the IKF (fixations and saccades) algorithm.}
    \item{\code{IMST}}{A numeric vector indicating the eye movement event classified by the IMST (fixations and saccades) algorithm.}
    \item{\code{IHMM}}{A numeric vector indicating the eye movement event classified by the IHMM (fixations and saccades) algorithm.}
    \item{\code{IVT}}{A numeric vector indicating the eye movement event classified by the IVT (fixations and saccades) algorithm.}
    \item{\code{NH}}{A numeric vector indicating the eye movement event classified by the NH (fixations, saccades, and PSOs) algorithm.}
    \item{\code{BIT}}{A numeric vector indicating the eye movement event classified by the BIT (only fixations) algorithm.}
    \item{\code{LNS}}{A numeric vector indicating the eye movement event classified by the LNS (saccades and PSOs) algorithm.}
  }
}
\details{
  The annotated and classified events are the following:
  \describe{
    \item{Not classified}{\code{0}}
    \item{Fixation}{\code{1}}
    \item{Saccade}{\code{2}}
    \item{Postsaccadic oscillation (PSO)}{\code{3}}
    \item{Smooth pursuit}{\code{4}}
    \item{Blink}{\code{5}}
    \item{Other}{\code{6}}
  }
  The data was recorded with 500 Hz and the stimuli were presented on a screen with resolution 1024x724 px and size 380x300 mm.     The viewing      distance was 670 mm.
}
\source{
Andersson, R., Larsson, L., Holmqvist, K., Stridh, M., & Nystrom, M. (2017). One
algorithm to rule them all? An evaluation and discussion of ten eye movement
event-detection algorithms. Behavior Research Methods, 49, 616-637.
https://doi.org/10.3758/s13428-016-0738-9

Data available at: https://github.com/richardandersson/EyeMovementDetectorEvaluation
}
\references{
Larsson, L., Nystrom, M., & Stridh, M. (2013). Detection of saccades and postsaccadic
oscillations in the presence of smooth pursuit. IEEE Transactions on Biomedical
Engineering, 60 (9), 2484–2493. https://doi.org/10.1109/TBME.2013.2258918

Larsson, L., Nyström, M., Andersson, R., & Stridh, M. (2015). Detection of fixations and
smooth pursuit movements in high-speed eye-tracking data. Biomedical Signal
Processing and Control, 18, 145–152. https://doi.org/10.1016/j.bspc.2014.12.008
}
\examples{
data(video)
}
\keyword{datasets}
