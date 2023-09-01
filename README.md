# Network Graph Simulation (NetGameSim) Toolkit
### Using 
![A generated graph with 300 nodes](./outputs/Graph300Nodes.png)

```hocon
NGSimulator {
    seed = 100
    outputDirectory = "/Users/drmark/github/NetGameSim/outputs"
    NetModel {
        dopplegangers = 10
        distanceSpreadThreshold = 0.05
        numberOfExperiments = 100
        perturbationCoefficient = 0.3
        dissimulationCoefficient = 0.15
        distanceCoefficient = 0.2
        edgeProbability = 0.001
        statesTotal = 100
        desiredReachabilityCoverage = 1.0
        numberOfWalks = 50
        maxBranchingFactor = 7
        maxDepth = 5
        maxProperties = 20
        propValueRange = 100
        actionType = 20
        actionRange = 10
        connectedness = 2
        maxWalkPathLengthCoeff = 2
        graphWalkTerminationPolicy = ${NGSimulator.Constants.MaxPathLength}
        graphWalkNodeTerminationProbability = 0.001d
    }
    CostRewards {
        malAppBudget = 2700.0
        costOfDetection = 0.2d
        serviceRewardProbability = 0.3
        serviceReward = 10d
        servicePenalty = 3d
        targetAppScore = 5
        targetAppLowPenalty = 1
        targetAppHighPenalty = 2
    }
    Constants {
       MaxPathLength  = "maxpathlength"
       UntilCycle = "untilcycle"
       All = "all"
    }
}
```

Once the repository is cloned, assuming its local path is the root directory ```/path/to/the/cloned/NetGameSim``` open a terminal window and switch to the directory ```cd /path/to/the/cloned/NetGameSim``` and build the project using the command ```sbt clean compile assembly``` that results in the executable file located under ```target/scala-3.2.2/netmodelsim.jar``` relative to the root directory. Once build you can execute the program using the following command ```java -Xms2G -Xmx30G -jar -DNGSimulator.NetModel.statesTotal=300  target/scala-3.2.2/netmodelsim.jar``` where you can modify the memory allocation values for the command line arguments Xms and Xmx. In addition, you can overwrite configuration options as it is shown with the option ```NGSimulator.NetModel.statesTotal``` that is located in the configuration file. 

You should make sure that the Java version that is used to compile this project matches the JVM version that is used to run the generated program jar from the command line, otherwise you may receive a variant of the following error message: "Exception in thread "main" java.lang.UnsupportedClassVersionError: org/jgrapht/Graph has been compiled by a more recent version of the Java Runtime (class file version 55.0), this version of the Java Runtime only recognizes class file versions up to 52.0." A quick check using the command ```java -version``` shows "openjdk version 1.8.0_292" meaning that we should switch to a higher version of the JVM. First, we check to see what JDKs are installed using the command ```/usr/libexec/java_home -V``` that outputs a list of versions like the following.
```shell
Matching Java Virtual Machines (6):
19.0.1 (x86_64) "Oracle Corporation" - "OpenJDK 19.0.1" /Users/drmark/Library/Java/JavaVirtualMachines/openjdk-19.0.1/Contents/Home
18.0.1.1 (x86_64) "Oracle Corporation" - "OpenJDK 18.0.1.1" /Users/drmark/Library/Java/JavaVirtualMachines/openjdk-18.0.1.1/Contents/Home
17.0.1 (x86_64) "Oracle Corporation" - "OpenJDK 17.0.1" /Users/drmark/Library/Java/JavaVirtualMachines/openjdk-17.0.1/Contents/Home
14.0.1 (x86_64) "Oracle Corporation" - "Java SE 14.0.1" /Library/Java/JavaVirtualMachines/jdk-14.0.1.jdk/Contents/Home
11.0.20.0-m1 (x86_64) "IBM Corporation" - "IBM Semeru Runtime Open Edition 11" /Users/drmark/Library/Java/JavaVirtualMachines/semeru-11.0.20/Contents/Home
1.8.0_292 (x86_64) "AdoptOpenJDK" - "AdoptOpenJDK 8" /Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home
```
To choose a higher version of the JVM we execute the following command ```export JAVA_HOME=`/usr/libexec/java_home -v 19.0.1` ``` that resolves the issue.