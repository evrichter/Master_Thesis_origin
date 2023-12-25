PennController.ResetPrefix(null);
DebugOff()

Sequence("consent", "consent2", "demographics", 
        "instructions",
        "practice", "practice_feedback", 
        "block1", "block1_feedback",
        "block2", "block2_feedback",
        "block3", "block3_feedback",
        "postexp_survey", SendResults(), "prolific_code")

var progressBarText = "Fortschritt. Entertaste: Nächster Text / Satz. Leertaste: Nächstes Wort.";
var showProgressBar = true;

// Consent 1
PennController("consent",
    newHtml("consent", "consent.html")
        .print()
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        .wait()
)
.log("survey", "CAPsurvey")

// Consent 2
PennController("consent2",
    newHtml("consent2", "consent2.html")
        .settings.checkboxWarning("Bitte stimmen Sie der Teilnahme zu.")
        .settings.inputWarning("Bitte geben Sie Ihre Prolific-ID an.")
        .log()
        .print()
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        // Continue only if the html has been filled in:
        .wait(
              getHtml("consent2").test.complete()
                  .failure(  getHtml("consent2").warn()  )
        )
        // .wait()
)
.log("survey", "CAPsurvey")

// Demographics
PennController("demographics",
    newHtml("demographics", "demographics.html")
        .settings.inputWarning("Bitte tragen Sie Ihre Muttersprache(n) und Ihr Alter ein.")
        .settings.radioWarning("Bitte geben Sie Ihr Geschlecht und Ihre Händigkeit an.")
        .log()
        .print()
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        // Continue only if the html has been filled in:
        .wait(
              getHtml("demographics").test.complete()
                  .failure(  getHtml("demographics").warn()  )
        )
        // .wait()
)
.log("survey", "CAPsurvey")

// Instructions
PennController("instructions",
    newHtml("instructions", "instructions.html")
        .print()
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas(200,150)
        .add(100,10, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(370,10, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(100,60, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(370,60, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 28px 10px 28px"}))
        .log()
        // .center()
        .print()
    ,
    newText("<p>In diesem Experiment gibt es insgesamt drei Blocks. Nach jedem Block erfahren Sie, wie gut Sie die Aufgabe bearbeitet haben. Zwischen den Blocks haben Sie Zeit, um eine kurze Pause zu machen.</p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newText("<p>Es geht nun los mit ein paar Übungstexten.</p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newButton("Übungstexte starten")
        .center()
        .bold()
        .print()
        .wait()
)
.log("survey", "CAPsurvey")

// Postexp
PennController("postexp_survey",
    newHtml("postexp_survey", "postexp_survey.html")
        .log()
        .print()
    ,
    newButton("Zum Prolific Code")
        .center()
        .bold()
        .print()
        .wait()
)
.log("survey", "CAPsurvey")

// Prolific Code
PennController("prolific_code",
    newHtml("prolific_code", "prolific_code.html")
        .log()
        .print()
    ,
    newButton("Experiment beenden.")
        .cssContainer({"font-size": "25px", "padding-top": "50px"})
        .center()
        .bold()
        .print()
        .wait()
)
.log("survey", "CAPsurvey")

// Initialize accuracy lists
Header(
    newVar("PracAcc", []).global()
    ,
    newVar("Block1Acc", []).global()
    ,
    newVar("Block2Acc", []).global()
    ,
    newVar("Block3Acc", []).global()
)

// Practice Presentation
Template("SPR_practice.csv", row =>
  newTrial("practice",
    newText("insSpace", "Drücken Sie Enter um den nächsten Übungstext zu lesen.")
        .cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px", "line-height": "400%"})
        .italic()
        .center()
        .print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("insSpace")
        .remove()
    ,
    // // Compute and print running accuracy (for debugging)
    // newVar("computedAcc").set(getVar("PracAcc")).set(v=>v.filter(a=>a===true).length/v.length),
    // ,
    // newText("accuracy").text(getVar("computedAcc")).print("left at 5px", "top at 5px")
    // ,
    //
    newText("MyContext", row.Context).cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "50px"}).print()
    ,
    //,
    newKey("Enter")
        .wait()
    ,
    getText("MyContext")
        .remove()
    ,
    newController("DashedSentence", {s: row.Sentence, mode:"self-paced reading", display: "in place", blankText: "#"})
        .center()
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
        .print()
        .log()
        .wait()
        .remove()
    ,
    newText("ComprehensionText", row.QuestionText)
    ,
    newText("LeftText", row.Left)
    ,
    newText("RightText", row.Right)
    ,
    newCanvas("ComprehensionQuestion", 400,400)
        .add(0,0, getText("ComprehensionText").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        // .log()
        // .print()
    ,
    newKey("ComprehensionDecide", "DK")
    ,
    newText("IsQuestion", row.Question)
        .test.text("yes")
            .success(
                getCanvas("ComprehensionQuestion").center()
                    .log()
                    .print()
                ,
                getKey("ComprehensionDecide").wait().log()
                    .test.pressed(row.QuestionCorrect)
                    .success( getVar("PracAcc").set(v=>[...v,true]) )
                    .failure( getVar("PracAcc").set(v=>[...v,false]) )
            )
  )
  .log("Item", row.ItemNum)
  .log("Condition", row.Cond)
  .log("List", row.Group)
  .log("Critical", row.Critical)
  .log("QuestionCorrect", row.QuestionCorrect)
  .log("QuestionText", row.QuestionText)
  .log("QuestionCondition", row.QuestionCond)
)

// Practice feedback
newTrial("practice_feedback",
    newText("Ende der Übungsphase")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,
    // Calculate mean accuracy
    newVar("meanPracAcc").set(getVar("PracAcc")).set(v=>v.filter(a=>a===true).length/v.length)
        .log()
    ,
    // Accuracy feedback
    newText("low_acc", "<b>Antwortgenauigkeit: <i>niedrig.</i></b> Tipp: Versuchen Sie, die Sätze noch etwas genauer zu lesen.")  // accuracy < 60%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("medium_acc", "<b>Antwortgenauigkeit: <i>in Ordnung.</i></b> Das klappt schon ganz gut!")  // 60% <= accuracy < 80%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("high_acc", "<b>Antwortgenauigkeit: <i>hoch.</i></b> Das klappt schon sehr gut, weiter so!")  // 80% <= accuracy
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    getVar("meanPracAcc").test.is( v => v < 0.6 )
        .success( getText("low_acc").print() )
        .failure(
          getVar("meanPracAcc").test.is( v => v < 0.8 )
            .success( getText("medium_acc").print() )
            .failure( getText("high_acc").print() )
          )
    ,
    newText("Die Übungsphase ist nun vorbei. Klicken Sie auf \"Experiment beginnen\" um fortzufahren.")
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px", "padding-bottom": "50px"})
        .center()
        .italic()
        .print()
    ,
    newButton("Experiment beginnen")
        .center()
        .bold()
        .print()
        .wait()
)

// Block 1
Template("SPR_o2_l2_b1.csv", row =>
  newTrial("block1",
    newText("insSpace", "Drücken Sie Enter um den nächsten Text zu lesen.")
        .cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px", "line-height": "400%"})
        .italic()
        .center()
        .print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("insSpace")
        .remove()
    ,
    newText("MyContext", row.Context).cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "50px"}).print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("MyContext")
        .remove()
    ,
    newController("DashedSentence", {s: row.Sentence, mode:"self-paced reading", display: "in place", blankText: "#"})
        .center()
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
        .print()
        .log()
        .wait()
        .remove()
    ,
    newText("ComprehensionText", row.QuestionText)
    ,
    newText("LeftText", row.Left)
    ,
    newText("RightText", row.Right)
    ,
    newCanvas("ComprehensionQuestion", 400,400)
        .add(0,0, getText("ComprehensionText").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
    ,
    newKey("ComprehensionDecide", "DK")
    ,
    newText("IsQuestion", row.Question)
        .test.text("yes")
            .success(
                getCanvas("ComprehensionQuestion")
                    .log()
                    .print()
                ,
                getKey("ComprehensionDecide").wait().log()
                    .test.pressed(row.QuestionCorrect)
                    .success( getVar("Block1Acc").set(v=>[...v,true]) )
                    .failure( getVar("Block1Acc").set(v=>[...v,false]) )
            )
  )
  .log("Item", row.ItemNum)
  .log("Condition", row.Cond)
  .log("List", row.Group)
  .log("Critical", row.Critical)
  .log("QuestionCorrect", row.QuestionCorrect)
  .log("QuestionText", row.QuestionText)
  .log("QuestionCondition", row.QuestionCond)
)

// Block1 feedback
newTrial("block1_feedback",
    newText("Ende des 1. Blocks")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,
    // Calculate mean accuracy
    newVar("meanBlock1Acc").set(getVar("Block1Acc")).set(v=>v.filter(a=>a===true).length/v.length)
        .log()
    ,
    // Accuracy feedback
    newText("low_acc", "<b>Antwortgenauigkeit: <i>niedrig.</i></b> Tipp: Versuchen Sie, die Texte und Sätze noch gründlicher zu lesen.")  // accuracy < 60%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("medium_acc", "<b>Antwortgenauigkeit: <i>mittel.</i></b> Nicht schlecht! Tipp: Versuchen Sie, jeden Satz gründlich zu lesen.")  // 60% <= accuracy < 80%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("high_acc", "<b>Antwortgenauigkeit: <i>hoch.</i></b> Bravo, weiter so!")  // 80% <= accuracy
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    getVar("meanBlock1Acc").test.is( v => v < 0.6 )
        .success( getText("low_acc").print() )
        .failure(
          getVar("meanBlock1Acc").test.is( v => v < 0.8 )
            .success( getText("medium_acc").print() )
            .failure( getText("high_acc").print() )
          )
    ,
    newText("Sie können nun eine kleine Pause machen. ☕️")
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px", "padding-bottom": "50px"})
        .center()
        .italic()
        .print()
    ,
    newButton("Pause beenden und Experiment fortsetzen")
        .center()
        .bold()
        .print()
        .wait()
)

// Block 2
Template("SPR_o2_l2_b2.csv", row =>
  newTrial("block2",
    newText("insSpace", "Drücken Sie Enter um den nächsten Text zu lesen.")
        .cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px", "line-height": "400%"})
        .italic()
        .center()
        .print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("insSpace")
        .remove()
    ,
    newText("MyContext", row.Context).cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "50px"}).print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("MyContext")
        .remove()
    ,
    newController("DashedSentence", {s: row.Sentence, mode:"self-paced reading", display: "in place", blankText: "#"})
        .center()
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
        .print()
        .log()
        .wait()
        .remove()
    ,
    newText("ComprehensionText", row.QuestionText)
    ,
    newText("LeftText", row.Left)
    ,
    newText("RightText", row.Right)
    ,
    newCanvas("ComprehensionQuestion", 400,400)
        .add(0,0, getText("ComprehensionText").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
    ,
    newKey("ComprehensionDecide", "DK")
    ,
    newText("IsQuestion", row.Question)
        .test.text("yes")
            .success(
                getCanvas("ComprehensionQuestion")
                    .log()
                    .print()
                ,
                getKey("ComprehensionDecide").wait().log()
                    .test.pressed(row.QuestionCorrect)
                    .success( getVar("Block2Acc").set(v=>[...v,true]) )
                    .failure( getVar("Block2Acc").set(v=>[...v,false]) )
            )
  )
  .log("Item", row.ItemNum)
  .log("Condition", row.Cond)
  .log("List", row.Group)
  .log("Critical", row.Critical)
  .log("QuestionCorrect", row.QuestionCorrect)
  .log("QuestionText", row.QuestionText)
  .log("QuestionCondition", row.QuestionCond)
)

// Block2 feedback
newTrial("block2_feedback",
    newText("Ende des 2. Blocks")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,
    // Calculate mean accuracy
    newVar("meanBlock2Acc").set(getVar("Block2Acc")).set(v=>v.filter(a=>a===true).length/v.length)
        .log()
    ,
    // Accuracy feedback
    newText("low_acc", "<b>Antwortgenauigkeit: <i>niedrig.</i></b> Tipp: Versuchen Sie, die Texte und Sätze noch gründlicher zu lesen.")  // accuracy < 60%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("medium_acc", "<b>Antwortgenauigkeit: <i>mittel.</i></b> Nicht schlecht! Tipp: Versuchen Sie, jeden Satz gründlich zu lesen.")  // 60% <= accuracy < 80%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("high_acc", "<b>Antwortgenauigkeit: <i>hoch.</i></b> Bravo, weiter so!")  // 80% <= accuracy
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    getVar("meanBlock2Acc").test.is( v => v < 0.6 )
        .success( getText("low_acc").print() )
        .failure(
          getVar("meanBlock2Acc").test.is( v => v < 0.8 )
            .success( getText("medium_acc").print() )
            .failure( getText("high_acc").print() )
          )
    ,
    newText("Sie können nun eine kleine Pause machen. ☕️")
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px", "padding-bottom": "50px"})
        .center()
        .italic()
        .print()
    ,
    newButton("Pause beenden und Experiment fortsetzen")
        .center()
        .bold()
        .print()
        .wait()
)

// Block 3
Template("SPR_o2_l2_b3.csv", row =>
  newTrial("block3",
    newText("insSpace", "Drücken Sie Enter um den nächsten Text zu lesen.")
        .cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px", "line-height": "400%"})
        .italic()
        .center()
        .print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("insSpace")
        .remove()
    ,
    newText("MyContext", row.Context).cssContainer({"font-family": "monospace", "font-size": "20px", "padding-top": "50px"}).print()
    ,
    newKey("Enter")
        .wait()
    ,
    getText("MyContext")
        .remove()
    ,
    newController("DashedSentence", {s: row.Sentence, mode:"self-paced reading", display: "in place", blankText: "#"})
        .center()
        .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
        .print()
        .log()
        .wait()
        .remove()
    ,
    newText("ComprehensionText", row.QuestionText)
    ,
    newText("LeftText", row.Left)
    ,
    newText("RightText", row.Right)
    ,
    newCanvas("ComprehensionQuestion", 400,400)
        .add(0,0, getText("ComprehensionText").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
    ,
    newKey("ComprehensionDecide", "DK")
    ,
    newText("IsQuestion", row.Question)
        .test.text("yes")
            .success(
                getCanvas("ComprehensionQuestion")
                    .log()
                    .print()
                ,
                getKey("ComprehensionDecide").wait().log()
                    .test.pressed(row.QuestionCorrect)
                    .success( getVar("Block3Acc").set(v=>[...v,true]) )
                    .failure( getVar("Block3Acc").set(v=>[...v,false]) )
            )
  )
  .log("Item", row.ItemNum)
  .log("Condition", row.Cond)
  .log("List", row.Group)
  .log("Critical", row.Critical)
  .log("QuestionCorrect", row.QuestionCorrect)
  .log("QuestionText", row.QuestionText)
  .log("QuestionCondition", row.QuestionCond)
)

// Block3 feedback
newTrial("block3_feedback",
    newText("Ende des 3. Blocks")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,
    // Calculate mean accuracy
    newVar("meanBlock3Acc").set(getVar("Block3Acc")).set(v=>v.filter(a=>a===true).length/v.length)
        .log()
    ,
    // Accuracy feedback
    newText("low_acc", "<b>Antwortgenauigkeit: <i>niedrig.</i></b> Tipp: Versuchen Sie, die Texte und Sätze noch gründlicher zu lesen.")  // accuracy < 60%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("medium_acc", "<b>Antwortgenauigkeit: <i>mittel.</i></b> Nicht schlecht! Tipp: Versuchen Sie, jeden Satz gründlich zu lesen.")  // 60% <= accuracy < 80%
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("high_acc", "<b>Antwortgenauigkeit: <i>hoch.</i></b> Bravo, weiter so!")  // 80% <= accuracy
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    getVar("meanBlock3Acc").test.is( v => v < 0.6 )
        .success( getText("low_acc").print() )
        .failure(
          getVar("meanBlock3Acc").test.is( v => v < 0.8 )
            .success( getText("medium_acc").print() )
            .failure( getText("high_acc").print() )
          )
    ,
    newText("Sie können nun eine kleine Pause machen. ☕️")
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px", "padding-bottom": "50px"})
        .center()
        .italic()
        .print()
    ,
    newButton("Pause beenden und Experiment fortsetzen")
        .center()
        .bold()
        .print()
        .wait()
)
