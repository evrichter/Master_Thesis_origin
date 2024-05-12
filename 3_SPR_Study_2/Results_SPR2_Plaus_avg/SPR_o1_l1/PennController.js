PennController.ResetPrefix(null);
DebugOff()
Sequence("counter","instructions",
        "practice", "practice_feedback",
        "BLOCK1", "BLOCK1feedback", 
        "Block1_instructions_word_recognition", "Block1_word_recognition", "Block1_feedback_word_recognition",
        "BLOCK2instructions", "BLOCK2practice", "BLOCK2",
        "Block2_instructions_word_recognition", "Block2_word_recognition", "Block2_feedback_word_recognition",
        "EXPfeedback", SendResults())

SetCounter("counter", "inc", 1);

// Instructions

PennController("instructions",
    newHtml("instructions", "instructions.html")
        .print()
    ,
    newText("LeftText", "Nein") // This is only a new element. It isn't used at this point (no .print())
    ,
    newText("RightText", "Ja")
    ,
    newCanvas(200,150) // Canvas elements give us more control over how content is displayed. The numbers are coordinates.
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
// In the canvas above, some elements are added (.add()) directly, others are called from previous created elements. This is important if you want to (pseudo-)randomize the button positions.

    newText("<p></p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newText("<p>Es geht nun los mit einem Übungstext.</p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newButton("Übungstext starten")
        .center()
        .bold()
        .print()
        .wait()
)

// Initialize accuracy lists
Header(
    newVar("PracAcc", []).global()
    ,
    newVar("EXPAcc", []).global()
    ,
    newVar("RecognitionAcc1", []).global()
    ,
    newVar("DeepAcc", []).global()
    ,
    newVar("RecognitionAcc2", []).global()
)

// Practice Block1: shallow
Template("practice.csv", row =>// This csv contains a column Group with different entries for the lists.
  newTrial("practice",

    newText("Question", row.question)
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas("ComprehensionQuestion", 400,400) // the numerics define the size (x and y axis)
        .add(0,0, getText("Question").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .center()
    ,
    
    newKey("ComprehensionDecide", "DK") // This defines which buttons are allowed
    ,
    
    newText("Condition", row.condition)
    .test.text("shallow")
    .success(
    newText("IsText", row.istext)
    .test.text("text")
        .success(newController("DashedSentence", {s: row.text, mode:"self-paced reading", display: "in place", blankText: "#"}) //
            .center()
            .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
            .print()
            .log()
            .wait()
            .remove()
            )
        .failure(
            getText("IsText").test.text("question")
            .success(
                getCanvas("ComprehensionQuestion")
                .log()
                .print()
                ,
                getKey("ComprehensionDecide")
                    .log()
                    .wait()
                    .test.pressed(row.answer)
                    .success( getVar("PracAcc").set(v=>[...v,true]))
                    .failure( getVar("PracAcc").set(v=>[...v,false]))
                        )
        ))
    )
  
// tokenindex,filname,filegenre,istext,text,condition,question,answer
  .log("WordNo", row.tokenindex)
  .log("FileName", row.filname)
  .log("Genre", row.filegenre)
  .log("IsText", row.istext)
  .log("Text", row.text)
  .log("Condition", row.condition)
  .log("Question", row.question)
  .log("Answer", row.answer)
  .log("Explanation", row.explanation)
)
,

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
    newText("low_acc", "<b>Antwortgenauigkeit: <i>keine richtige Antwort.</i></b> Tipp: Versuchen Sie, die Sätze noch etwas genauer zu lesen.")  // accuracy < 60%
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
    getVar("meanPracAcc").test.is( v => v < 0.5 )
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
,
Template("fulllist_shallow.csv", row =>// This csv contains a column Group with different entries for the lists.
  newTrial("BLOCK1",
  
  
    newText("Question", row.question)
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas("ComprehensionQuestion", 400,400) // the numerics define the size (x and y axis)
        .add(0,0, getText("Question").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .center()
    ,
    newKey("ComprehensionDecide", "DK") // This defines which buttons are allowed
    
    ,
    
    newVar("Id", 1).global()

    ,
    
    newText("WordNo", row.tokenindex) //makes # disappear? idk how to fix
        .test.text(getVar("Id"))
        .failure(
                newText("Nexttext","<p><b>Beginnen wir nun mit einem neuen Text</b></p><br>Drücken Sie die Leertaste, um fortzufahren")
                    .cssContainer({"font-family": "serif", "font-size": "20px", "padding":"50px"})
                    .center()
                    .print()
                ,
                newKey(" ").wait()
                ,
                getText("Nexttext").remove()
                ,
                getVar("Id").set(v => v+1)
                )
    ,
    
    newText("IsText", row.istext)
    .test.text("text")
        .success(newController("DashedSentence", {s: row.text, mode:"self-paced reading", display: "in place", blankText: "#"}) //
            .center()
            .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
            .print()
            .log()
            .wait()
            .remove()
            )
        .failure(
            getText("IsText").test.text("question")
            .success(
                getCanvas("ComprehensionQuestion")
                .log()
                .print()
                ,
                getKey("ComprehensionDecide")
                    .log()
                    .wait()
                    .test.pressed(row.answer)
                    .success( getVar("EXPAcc").set(v=>[...v,true]))
                    .failure( getVar("EXPAcc").set(v=>[...v,false]))
                        )
        )
    )
  
// tokenindex,filname,filegenre,istext,text,condition,question,answer
  .log("WordNo", row.tokenindex)
  .log("FileName", row.filname)
  .log("Genre", row.filegenre)
  .log("IsText", row.istext)
  .log("Text", row.text)
  .log("Condition", row.condition)
  .log("Question", row.question)
  .log("Answer", row.answer)
)

,

// Block1 feedback
newTrial("BLOCK1feedback",
    newText("Ende des ersten Blocks")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,
    newVar("MeanAcc")
        .set(getVar("EXPAcc")).set(v=>v.filter(a=>a===true).length/v.length).log()
    ,
    newVar('grandaveragepercent')
        .set(getVar('MeanAcc'))
        .set(v => v * 100)
    ,
    newText()
        .text(getVar("grandaveragepercent"))
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .center()
        .before(newText('Ihre Genauigkeit lag bei '))
        .after(newText('%.'))
        .print()
    ,
    newText("<p></p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        .wait()
)
,

// Block 1 Word recognition Task
PennController("Block1_instructions_word_recognition",
    newHtml("Block1_instructions_word_recognition", "instructions_word_recognition.html")
        .print()
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        .wait()
)
,

Template("list_word_recognition.csv", row => 
  newTrial("Block1_word_recognition",

    newText("Word", row.Word_recognition)
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas("WordRecognitionQuestion", 400,400) // the numerics define the size (x and y axis)
        .add(0,0, getText("Word").center().italic().bold().cssContainer({"font-family": "monospace", "font-size": "30px", "padding-top": "50px", 
            "padding-left": "30%"
        }))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 0px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 0px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "20px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "20px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .center()
    ,
    newKey("WordRecognitionQuestionDecide", "DK") // This defines which buttons are allowed
    ,
    
    getCanvas("WordRecognitionQuestion")
        .log()
        .center()
        .print()
    ,
    
    getKey("WordRecognitionQuestionDecide")
        .log()
        .wait()
        .test.pressed(row.answer)
        .success( getVar("RecognitionAcc1").set(v=>[...v,true]))
        .failure( getVar("RecognitionAcc1").set(v=>[...v,false]))
    )
    .log("Word_recognition", row.Word_recognition)
)
,

// Word Recognition Task Block 1
newTrial("Block1_feedback_word_recognition",
    newText("Ende der Aufgabe zur Worterkennung")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,

    newVar("MeanAcc")
        .set(getVar("RecognitionAcc1")).set(v=>v.filter(a=>a===true).length/v.length).log()
    ,
    newVar('grandaveragepercent')
        .set(getVar('MeanAcc'))
        .set(v => v * 100)
    ,
    newText()
        .text(getVar("grandaveragepercent"))
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .center()
        .before(newText('Ihre Genauigkeit lag bei '))
        .after(newText('%.'))
        .print()
    
    ,
    newButton("Mit Block 2 starten")
        .center()
        .bold()
        .print()
        .wait()
)
,

PennController("BLOCK2instructions",
    newHtml("instructions2", "instructions2.html")
        .print()
    ,
// In the canvas above, some elements are added (.add()) directly, others are called from previous created elements. This is important if you want to (pseudo-)randomize the button positions.

    newText("<p></p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newText("<p>Es geht nun los mit einem Übungstext.</p>")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .left()
        .print()
    ,
    newButton("Übungstext starten")
        .center()
        .bold()
        .print()
        .wait()
)
,

// Practice Block2: deep
Template("practice.csv", row =>// This csv contains a column Group with different entries for the lists.
  newTrial("BLOCK2practice",

    newText("Question", row.question)
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas("ComprehensionQuestion", 400,400) // the numerics define the size (x and y axis)
        .add(0,0, getText("Question").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .center()
    ,
    
    newKey("ComprehensionDecide", "DK") // This defines which buttons are allowed
    ,
    
    //deep practice feedback
    newText("correct_answer", "<b>Richtig!</b>")
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    newText("wrong_answer", "<b>Falsch:</b>")
        .cssContainer({"font-family": "serif", "font-size": "20px", "padding-top": "50px"})
        .center()
    ,
    
    newText("Condition", row.condition)
    .test.text("deep")
    .success(
    newText("IsText", row.istext)
    .test.text("text")
        .success(newController("DashedSentence", {s: row.text, mode:"self-paced reading", display: "in place", blankText: "#"}) //
            .center()
            .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
            .print()
            .log()
            .wait()
            .remove()
            )
        .failure(
            getText("IsText").test.text("question")
            .success(
                getCanvas("ComprehensionQuestion")
                .log()
                .print()
                ,
                getKey("ComprehensionDecide")
                    .log()
                    .wait()
                    .test.pressed(row.answer)
                    .success(
                        getCanvas("ComprehensionQuestion").remove()
                        ,
                        getVar("PracAcc").set(v=>[...v,true])
                        ,
                        getText("correct_answer").print()
                        ,
                        newKey(" ").wait()
                    )
                    .failure(
                        getCanvas("ComprehensionQuestion").remove()
                        ,
                        getVar("PracAcc").set(v=>[...v,false])
                        ,
                        getText("wrong_answer").print()
                        ,
                        newText()
                            .text(row.explanation)
                            .cssContainer({"font-family": "serif", "font-size": "20px"})
                            .center()
                            .italic()
                            .before( newText("\"") )
                            .after( newText("\"") )
                            .print()
                        ,
                        newKey(" ").wait()
                    )
            )
        )
    )
  )
// tokenindex,filname,filegenre,istext,text,condition,question,answer
  .log("WordNo", row.tokenindex)
  .log("FileName", row.filname)
  .log("Genre", row.filegenre)
  .log("IsText", row.istext)
  .log("Text", row.text)
  .log("Condition", row.condition)
  .log("Question", row.question)
  .log("Answer", row.answer)
  .log("Explanation", row.explanation)
)

,

Template("fulllist_deep.csv", row =>// This csv contains a column Group with different entries for the lists.
  newTrial("BLOCK2",
  
    newText("Question", row.question)
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas("ComprehensionQuestion", 400,400) // the numerics define the size (x and y axis)
        .add(0,0, getText("Question").center().italic().cssContainer({"font-family": "monospace", "font-size": "19px", "padding-top": "50px"}))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 10px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "30px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .center()
    ,
    newKey("ComprehensionDecide", "DK") // This defines which buttons are allowed
    ,
    
    newVar("Id", 1).global()

    ,
    
    newText("WordNo", row.tokenindex) //makes # disappear? idk how to fix
        .test.text(getVar("Id"))
        .failure(
                newText("Nexttext","<p><b>Beginnen wir nun mit einem neuen Text</b></p><br>Drücken Sie die Leertaste, um fortzufahren")
                    .cssContainer({"font-family": "serif", "font-size": "20px", "padding":"50px"})
                    .center()
                    .print()
                ,
                newKey(" ").wait()
                ,
                getText("Nexttext").remove()
                ,
                getVar("Id").set(v => v+1)
                )
    ,
    
    newText("IsText", row.istext)
    .test.text("text")
        .success(newController("DashedSentence", {s: row.text, mode:"self-paced reading", display: "in place", blankText: "#"}) //
            .center()
            .cssContainer({"font-family": "monospace", "font-size": "25px", "padding-top": "50px"})
            .print()
            .log()
            .wait()
            .remove()
            )
        .failure(
            getText("IsText").test.text("question")
            .success(
                getCanvas("ComprehensionQuestion")
                .log()
                .print()
                ,
                getKey("ComprehensionDecide")
                    .log()
                    .wait()
                    .test.pressed(row.answer)
                    .success( getVar("EXPAcc").set(v=>[...v,true])
                        ,
                        getVar("DeepAcc").set(v=>[...v,true]))
                    .failure( getVar("EXPAcc").set(v=>[...v,false])
                        ,
                        getVar("DeepAcc").set(v=>[...v,false]))
                    )
            .failure(
                newVar("MeanDeepAcc")
                    .set(getVar("DeepAcc")).set(v=>v.filter(a=>a===true).length/v.length).log()
                ,
                 newVar('Deep_grandaveragepercent')
                    .set(getVar('MeanDeepAcc'))
                    .set(v => v * 100)
                ,
                newText()
                    .text(getVar("Deep_grandaveragepercent"))
                    .cssContainer({"font-family": "serif", "font-size": "20px", "padding": "50px"})
                    .center()
                    .before(newText('Ihre Genauigkeit lag bei '))
                    .after(newText('%.'))
                    .print()
                ,
                getVar("DeepAcc").set([])
                ,
                newKey(" ").wait()
                )
        )
    )
  
// tokenindex,filname,filegenre,istext,text,condition,question,answer
  .log("WordNo", row.tokenindex)
  .log("FileName", row.filname)
  .log("Genre", row.filegenre)
  .log("IsText", row.istext)
  .log("Text", row.text)
  .log("Condition", row.condition)
  .log("Question", row.question)
  .log("Answer", row.answer)
)
,

// Block 2 Word recognition Task
PennController("Block2_instructions_word_recognition",
    newHtml("Block2_instructions_word_recognition", "instructions_word_recognition.html")
        .print()
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        .wait()
)
,

Template("list2_word_recognition.csv", row => 
  newTrial("Block2_word_recognition",

    newText("Word", row.Word_recognition)
    ,
    newText("LeftText", "Nein")
    ,
    newText("RightText", "Ja")
    ,
    newCanvas("WordRecognitionQuestion", 400,400) // the numerics define the size (x and y axis)
        .add(0,0, getText("Word").center().italic().bold().cssContainer({"font-family": "monospace", "font-size": "30px", "padding-top": "50px", 
            "padding-left": "30%"
        }))
        .add(30,150, newText("Taste&nbsp;D").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 0px"}))
        .add(300,150, newText("Taste&nbsp;K").cssContainer({"font-family": "monospace", "font-size": "15px", "padding": "20px 10px 10px 0px"}))
        .add(30,200, getText("LeftText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "20px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .add(300,200, getText("RightText").bold().cssContainer(
            {"font-family": "monospace", "font-size": "20px", "border": "solid 1px black", "padding": "10px 10px 10px 10px"}))
        .center()
    ,
    newKey("WordRecognitionQuestionDecide", "DK") // This defines which buttons are allowed
    ,
    
    getCanvas("WordRecognitionQuestion")
        .log()
        .center()
        .print()
    ,
    
    getKey("WordRecognitionQuestionDecide")
        .log()
        .wait()
        .test.pressed(row.answer)
        .success( getVar("RecognitionAcc2").set(v=>[...v,true]))
        .failure( getVar("RecognitionAcc2").set(v=>[...v,false]))
    )
    .log("Word_recognition", row.Word_recognition)
)
,

// Word Recognition Task Block 2
newTrial("Block2_feedback_word_recognition",
    newText("Ende der Aufgabe zur Worterkennung")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,

    newVar("MeanAcc")
        .set(getVar("RecognitionAcc2")).set(v=>v.filter(a=>a===true).length/v.length).log()
    ,
    newVar('grandaveragepercent')
        .set(getVar('MeanAcc'))
        .set(v => v * 100)
    ,
    newText()
        .text(getVar("grandaveragepercent"))
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .center()
        .before(newText('Ihre Genauigkeit lag bei '))
        .after(newText('%.'))
        .print()
    
    ,
    newButton("Fortfahren")
        .center()
        .bold()
        .print()
        .wait()
)
,

newTrial("EXPfeedback",
    newText("Ende des zweiten Blocks")
        .cssContainer({"font-family": "serif", "font-size": "30px", "padding-top": "50px"})
        .bold()
        .center()
        .print()
    ,
    
    newText("<p>Vielen Dank für Ihre Teilnahme.")
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .center()
        .print(),
    
    newVar("MeanAcc")
        .set(getVar("EXPAcc")).set(v=>v.filter(a=>a===true).length/v.length).log()
    ,
     newVar('grandaveragepercent')
        .set(getVar('MeanAcc'))
        .set(v => v * 100)
    ,
    newText()
        .text(getVar("grandaveragepercent"))
        .cssContainer({"font-family": "serif", "font-size": "20px"})
        .center()
        .before(newText('Ihre allgemeine Genauigkeit lag bei '))
        .after(newText('%.'))
        .print()
    ,    
    newButton("Nachbefragung beginnen")
        .center()
        .bold()
        .print()
        .wait()
)
