library(mixtools)
courses.data <- scan("course_info_slim.txt", what="list")
courses.factors <- as.factor(courses.data)
students.data <- readLines("user_courses.txt")
students.data <- students.data[1:2000]
nstudents <- length(students.data)
ncourses <- length(courses.data)
students.courses <- array(dim=c(nstudents, ncourses))
for(i in 1:nstudents) {
    current.student = students.data[i]
    classes <- c()
    while(attr(comma.loc <- regexpr(",", current.student), "match.length") != -1) {
        classes <- c(classes, substr(current.student, start=1, stop=comma.loc[1] - 1))
        current.student <- substring(current.student, comma.loc[1] + 1)
    }
    nclasses = length(classes)
    students.courses[i,0:nclasses] <- classes
    # students.courses[i, -(1:nclasses)]
}
students.courses.matrix <- matrix(data=0, nrow=nstudents, ncol=ncourses)
for(i in 1:nstudents) {
    for(j in 1:ncourses) {
        current.course = courses.data[j]
        
        taken = FALSE

        for(chosen.course in students.courses[i,]) {
            # could also use grep and check whether -1 is returned
            # probably fastest to grep each chosen.course, add a 1 in the
            # returned index (if != -1) and then zero all remaining entries
            if((!is.na(chosen.course)) & (chosen.course == current.course)) {
                taken = TRUE
            }
        }
        if(taken) {
            students.courses.matrix[i,j] = 1
        }
        else {
            students.courses.matrix[i,j] = 0
        }
    }
}
ncomp = 10
results <- multmixEM(y=students.courses.matrix, k=ncomp)
ntop = 5
reduced.output.indices = array(data=0, dim=c(ncomp, ntop))
results.theta.copy = results$theta
for(i in 1:ncomp) {
    for(j in 1:ntop) {
        # find max, set to zero
        # in this way, grab top "ntop" indices
        # from each component
        reduced.output.indices[i,j] = which.max(results.theta.copy[i,])
        results.theta.copy[i, reduced.output.indices[i,j]] = 0
    }
}
reduced.output.courses = aperm(array(courses.data[reduced.output.indices], dim=c(ncomp, ntop)), c(2,1))
