#' Example of WHO Model Disability Survey data for adults
#'
#' An dummy data set of data from the WHO Model Disability Survey. All survey variables and demographic characterstics (except work status) are randomly generated. Responses to the survey questions and work status are from randomly selected rows of the 2014 Chilean implementation of the MDS, ENDIS II (2014).
#'
#' @format A tibble with 2500 rows and 90 variables:
#' \describe{
#'   \item{HHID}{household number}
#'   \item{strata}{survey strata}
#'   \item{PSU}{PSU id}
#'   \item{weight}{survey weights}
#'   \item{sex}{sex, Male or Female}
#'   \item{age}{age in years}
#'   \item{age_cat}{age catgory, one of "18-24", "25-39", "40-64", or "64-100"}
#'   \item{edu_cat}{highest level of education attained}
#'   \item{work_cat}{whether respondent is currently working, binary}
#'   \item{disability_score}{metric scale of performance, from Rasch analysis}
#'   \item{disability_cat}{performance level, as definted by WHO cutoffs of disability_score}
#'   \emph{Functioning section: On a scale from 1 (none) to 5 (extreme), in the last 30 days, taking into account any help or support you receive, how much has of a problem has it been for you to...}
#'   \item{F1}{...stand up from sitting?}
#'   \item{F2}{...stand for long periods of time, for example 30 minutes?}
#'   \item{F3}{...leave the house?}
#'   \item{F4}{...walk short distances, such as a street block or 100 meters?}
#'   \item{F5}{...walk 10 blocks or a kilometer?}
#'   \item{F6}{...do vigorous activities, such as play footbal, lift heavy objects, ride a bike or run?}
#'   \item{F7}{...get where you want to go?}
#'   \item{F8}{...manipulate small objects or opening a container?}
#'   \item{F9}{...lift a 2L full bottle of water from your waist to eye level?}
#'   \item{F10}{...wash or dress yourself?}
#'   \item{F11}{...feed yourself?}
#'   \item{F12}{...use the bathroom?}
#'   \item{F13}{...cut your toenails?}
#'   \item{F14}{...take care of your health, for example exercise, eat well or take your medications?}
#'   \item{F15}{...see objects at a distance?}
#'   \item{F16}{...see objects at arms length?}
#'   \item{F17}{...hear a conversation in a quiet room?}
#'   \item{F18}{...hear a conversation in a loud room?}
#'   \item{F19}{...feel pain?}
#'   \item{F20}{...sleep?}
#'   \item{F21}{...feel tired or not have enough energy?}
#'   \item{F22}{...feel short of breath?}
#'   \item{F23}{...cough or wheeze?}
#'   \item{F24}{...feel sad, down or depressed?}
#'   \item{F25}{...feel worried, nervous or anxious?}
#'   \item{F26}{...get along with people close to you, including your family and friends?}
#'   \item{F27}{...get along with people you don't know?}
#'   \item{F28}{...make new friends or maintain your friendships?}
#'   \item{F29}{...have intimiate relationships?}
#'   \item{F30}{...manage stress?}
#'   \item{F31}{...cope with everything you have to do?}
#'   \item{F32}{...be understood in your usual language?}
#'   \item{F33}{...understand others in your usual language?}
#'   \item{F34}{...forget things?}
#'   \item{F35}{...remember the important things you have to do day-to-day?}
#'   \item{F36}{...find solutions to your day-to-day problems?}
#'   \item{F37}{...complete household tasks, like sweeping, cooking, arranging the house or taking our the trash?}
#'   \item{F38}{...manage the money you have?}
#'   \item{F39}{...do things for relaxation?}
#'   \item{F40}{...participate in community activities?}
#'   \item{F41}{...participate in local or national policitics or civil society?}
#'   \item{F42}{...take care of others?}
#'   \item{F43}{...get a job?}
#'   \item{F44}{...obtain a higher education?}
#'   \item{F45}{...use public transportation?}
#'   \item{F46}{...get things done in your job (if not currently working, NA)?}
#'   \item{F47}{...get things done at your school (if not currently studying, NA)?}
#'   \emph{Capacity section: On a scale from 1 (none) to 5 (extreme), in the last 30 days, withing taking into account any type of help or support, due to your health how much difficulty have you had with...}
#'   \item{C2}{...seeing, without contact lenses or glasses?}
#'   \item{C3}{...hearing, without hearing aids?}
#'   \item{C4}{...walking or climbing steps?}
#'   \item{C5}{...remembering or concentrating?}
#'   \item{C6}{...washing or dressing?}
#'   \item{C7}{...communicating in your usual language?}
#'   \item{C8}{...feeding yourself?}
#'   \item{C9}{...using the bathroom?}
#'   \item{C10}{...waking up and getting out of bed?}
#'   \item{C11}{...going out to the street?}
#'   \item{C12}{...doing shopping or going to the doctor?}
#'   \item{C13}{...manipulating small objects or opening a container?}
#'   \item{C14}{...sleeping?}
#'   \item{C15}{...breathing?}
#'   \item{C16}{...doing household tasks like sweeping, cooking, arranging the house or taking out the trash?}
#'   \item{C17}{...taking care of others?}
#'   \item{C18}{...participating in community activities?}
#'   \item{C19}{...feeling sad, down or depressed?}
#'   \item{C20}{...feeling worried, nervous or anxious?}
#'   \item{C21}{...getting along with people close to you, including your family and friends?}
#'   \item{C22}{...coping with everything you have to do?}
#'   \item{C23}{...feeling pain?}
#'   \item{C24}{...getting things done in your job (if not currently working, NA)?}
#'   \item{C25}{...getting things done at your school (if not currently studying, NA)?}
#'   \emph{Environmental factors section: On a scale from 1 (very easy) to 5 (very hard), to what extent...}
#'   \item{EF1}{...do health facilities you need regularly make it easy or hard for you to use them?}
#'   \item{EF2}{...do places where you socialize and engage in community activities make it easy or hard for you to do this?}
#'   \item{EF3}{...do the shops, banks and post office in your neighbourhood make it easy or hard for you to use them?}
#'   \item{EF4}{...do your regular places of worship make it easy or hard for you to worship?}
#'   \item{EF5}{...does the transportation you need or want to use make it easy or hard for you to use it?}
#'   \item{EF6}{...does your dwelling (including the toilet and all rooms) make it easy or hard for you to live there?}
#'   \item{EF7}{...do the temperature, terrain, and climate of the place you usually live make it easy or hard for you to live there?}
#'   \item{EF8}{...does the lighting in your surroundings make it easy or hard for you to live there?}
#'   \item{EF9}{...does the noise in your surroundings make it easy or hard for you to live there?}
#'   \item{EF10}{...do the crowds in your surroundings make it easy or hard for you to live there?}
#'   \item{EF11}{...does your workplace make it easy or hard for you to work or learn (if not currently working, NA)?}
#'   \item{EF12}{...does your educational institution make it easy or hard for you to work or learn (if not currently studying, NA)?}
#' }
"df_adults"

#' Example of WHO Model Disability Survey data for children
#'
#' An dummy data set of data from the WHO Model Disability Survey for children.  All survey variables and demographic characterstics (except age variables) are randomly generated. Responses to the survey questions and age variables are from randomly selected rows of the 2014 Chilean implementation of the MDS, ENDIS II (2014).
#'
#' @format A tibble with 2500 rows and 42 variables:
#' \describe{
#'   \item{HHID}{household number}
#'   \item{strata}{survey strata}
#'   \item{PSU}{PSU id}
#'   \item{weight}{survey weights}
#'   \item{sex}{sex, Male or Female}
#'   \item{age}{age in years}
#'   \item{age_cat}{age catgory, one of "Age2to4", "Age5to9", or "Age10to17"}
#'   \emph{Functioning section: On a scale from 1 (none) to 5 (extreme), in the last 30 days, taking into account any help or support the child receives, compared with children of the same age, how much has of a problem it been for the child to...}
#'   \item{child1}{...walk?}
#'   \item{child2}{...manipulat small objects or opening a container?}
#'   \item{child3}{...see things from a long distance?}
#'   \item{child4}{...hear?}
#'   \item{child5}{...feel pain?}
#'   \item{child6}{...not have enough energy?}
#'   \item{child7}{...have too much energy}
#'   \item{child8}{...feel short of breath?}
#'   \item{child9}{...feel sad, down or depressed?}
#'   \item{child10}{...feel worried, nervous or anxious?}
#'   \item{child11}{...(for children aged 5 to 17) wash or dress him- or herself?}
#'   \item{child12}{...(for children aged 2 to 4) bite or hit other children or adults?}
#'   \item{child13}{...(for children aged 5 to 17) control his or her own behavior?}
#'   \item{child14}{...(for children aged 5 to 17) get along with children of the same age?}
#'   \item{child15}{...(for children aged 2 to 4) understand what you say to him or her?}
#'   \item{child16}{(for children aged 2 to 4) How much of a problem has it been for you to understand what the child says?}
#'   \item{child17}{...(for children aged 5 to 17) understand other people?}
#'   \item{child18}{...(for children aged 5 to 17) be understood?}
#'   \item{child19}{...(for children aged 2 to 3) learn the names of household objects?}
#'   \item{child20}{...(for children aged 3 to 17) learn to do new things?}
#'   \item{child21}{...(for children aged 5 to 17) complete a task?}
#'   \item{child22}{...(for children aged 5 to 17) make changes to his or her routine?}
#'   \item{child23}{...(for children aged 5 to 17) do homework as requested at school?}
#'   \item{child24}{...(for children aged 2 to 5) play with toys or domestic objects?}
#'   \item{child25}{...(for children aged 2 to 12) play with other children?}
#'   \item{child26}{...(for children aged 13 to 17) do activities with other children?}
#'   \item{child27}{...(for children aged 5 to 17) participate in community activities?}
#'   \emph{Capacity section: On a scale from 1 (none) to 5 (extreme), in the last 30 days, without taking into account any help or support the child receives, how much has difficulty has the child had with...}
#'   \item{child28}{...seeing without glasses?}
#'   \item{child29}{...hearing without hearing aids?}
#'   \item{child30}{...walking?}
#'   \item{child31}{...understanding you or others?}
#'   \item{child32}{...learning?}
#'   \item{child33}{...controling his or her behavior?}
#'   \item{child34}{...completing a task?}
#'   \item{child35}{...getting along with other children?}
#' }
"df_children"