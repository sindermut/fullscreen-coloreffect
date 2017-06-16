#NoEnv
#SingleInstance Force
SetBatchLines, -1
SetWorkingDir, % A_ScriptDir
SetFormat, FloatFast, 0.10

global Config := { ActiveOnStartup:true
				 , ExitOnClose:true
				 , SmoothToggles:true
				 , SmoothTransitions:true
				 , Exit:"!+F8"
				 , Toggle:"^+F8"
				 , ConfigFile:"NegativeScreen.conf"
				 , SaveFile:"ColorEffect.dat" }
, gHwnd := { GUI:0, Tab:0, Slider:0, ButtonToggle:0, ButtonApply:0, CheckboxInvert:0, ConfigEdit:0 }
, ListBox := [ { Name:"Mode", Hwnd:0, Content:"", Matrix:{}, Hotkey:{} } ]
, EditField := { Name:[], Hwnd:[], Content:[], DefaultContent:[] }

try
{
	ColorMatrix := new ColorMatrix
	ColorEffect := new ColorEffect
	Brightness := new Brightness
	Initialize()
}
catch e
{
	MsgBox % e.What ": " e.Message " (Line " e.Line ")"
	ExitApp
}

; should work fine for normal user
ParseConfiguration()
{
	Loop, Read, % Config.ConfigFile
	{
		; unrestrictive parser, something like Ke"y=Val"ue is valid
		; remove anything after '#' not enclosed in quotes except #=== at the beginning
		RegExMatch(A_LoopReadLine, "S)(\s*#={3})?((?:[^#""]|""[^""]*""|""[^#]*)*)", lineStr)
		; remove enclosing quote, replace two consecutive quote with linefeed, trim whitespaces and remove all quote, replace linefeed with quote, quote quote quote
		lineStr := StrReplace(RegExReplace(StrReplace(RegExReplace(lineStr, "S)(?|(.*?)""(.*)""(.*)|(.*))", "$1$2$3"), """""", "`n"), "S)^\s+|\s+$|"""), "`n", """")
		
		if RegExMatch(lineStr, "^#={3}\K.*", match) ; #===Listbox Name
		{
			if (ListBox[boxIndex].Content = "")
				ListBox.RemoveAt(boxIndex--)
			
			for boxIndex, v in ListBox
				continue
			until (v.Name = match)
			if (v.Name != match) && (ListBox[++boxIndex].Content = "")
				ListBox[boxIndex] := {Name:match, Hwnd:0, Content:"", Matrix:{}, Hotkey:{}}
			else
				ListBox[boxIndex].Name := match
		}
		else if RegExMatch(lineStr, "(.*?)\s*=\s*(.*)", match) ; variable=value
		{
			boxIndex := boxIndex < 1 ? 1 : boxIndex
			; try to convert string into hotkey
			parsedHotkey := parsedHotkey_m := parsedHotkey_k := ""
			Loop, Parse, match2, +
			{
				if RegExMatch(A_LoopField, "i)^(l|r)?(ctrl|shift|alt|win)", k) ; modifiers
					parsedHotkey_m .= (k1="l"?"<" : k1="r"?">" : "") . (k2="ctrl"?"^" : k2="shift"?"+" : k2="alt"?"!" : "#")
				else if RegExMatch(A_LoopField, "i)^D\K\d$", match) ; D0 - D9
					parsedHotkey_k := match
				else if GetKeyVK(A_LoopField) ; assuming key name
					parsedHotkey_k := A_LoopField
				else if ((match := GetKeyName(Format("VK{:x}", A_LoopField))) != "") ; assuming virtual key number
					parsedHotkey_k := match
			}
			parsedHotkey := (parsedHotkey_k != "") ? parsedHotkey_m . parsedHotkey_k : ""
			
			if config.HasKey(match1) ; valid configuration option
				if match1 in Exit,Toggle
					config[match1] := parsedHotkey
				else
					config[match1] := match2 ~= "i)^(false|no|0+)$" ? false : match2 ~= "i)^(true|yes|\d+)$" ? true : config[match1]
			else
				currentItemName := match1, parsedMatrix := [], (match2 ~= "^{.*\d.*}") ? lineStr := match2 : ListBox[boxIndex].Hotkey[currentItemName] := parsedHotkey
		}
		if (lineStr ~= "^{.*\d.*}") ; {matrix row}
		{
			match2 := RegExReplace(lineStr, "\s+")
			; [optionally prepended with + or -], match ( 1 | 1. | .1 | 1.0 | 1.e1 ), not ( 0x1 | 1e1 | .e1 | +-1 | 1.e )
			while RegExMatch(match2, "S)[{,]([+-]?(?:\d*\.(?(?<=\d\.)(?i)e[+-]?)?\d+|\d+\.?))([,}].*)", match)
			{
				if (parsedMatrix.Push(match1) != 25)
					continue
				ListBox[boxIndex].Content .= currentItemName ""
				ListBox[boxIndex].Matrix[currentItemName] := ColorMatrix.ZeroMatrix
				for k, v in parsedMatrix
					ListBox[boxIndex].Matrix[currentItemName][(k-1)//5+1][Mod(k-1,5)+1] := v
			}
		}
	}
}
LoadSavedState()
{
	datafile := StrSplit(FileOpen(Config.SaveFile, "r").Read(), "")._NewEnum()
	
	if (datafile[0, str] && str == "ColorEffectDataFile")
	{
		datafile[0, lastTab]
		lastTab := StrSplit(lastTab, "")
		GuiControl, Choose, % gHwnd.Tab, % lastTab[1]
		GuiControl,, % gHwnd.CheckboxInvert, % lastTab[2] != 0
		
		datafile[0, lastSelection], lastSel := []
		Loop, Parse, lastSelection, 
			lastSel[A_Index] := StrSplit(A_LoopField, "")
		ListBox_SetSelection(lastSel)
		
		datafile[0, lastEditFieldContent]
		EditField_Set(StrSplit(lastEditFieldContent, "")*)
		
		datafile[0, lastPosition]
		VarSetCapacity(lastPos, 11*4, 0)
		Loop, Parse, lastPosition, 
			NumPut(A_LoopField, lastPos, (A_Index-1)*4, "Int")
		return DllCall("SetWindowPlacement", "Ptr", gHwnd.GUI, "Ptr", &lastPos)
	}
}
SaveCurrentState()
{
	datafile := "ColorEffectDataFile" ""
	
	GuiControlGet, lastTab,, % gHwnd.Tab
	GuiControlGet, lastCheckbox,, % gHwnd.CheckboxInvert
	datafile .= lastTab "" (lastCheckbox != 0) ""
	
	for index, list in ListBox_GetSelection()
		lastSelection .= StrConcat("", list*) ""
	datafile .= SubStr(lastSelection, 1, -1) ""
	
	datafile .= StrConcat("", EditField.Content*) ""
	
	VarSetCapacity(lastPos, 11*4, 0)
	DllCall("GetWindowPlacement", "Ptr", gHwnd.GUI, "Ptr", &lastPos)
	Loop 11
		lastPosition .= NumGet(lastPos, (A_Index-1)*4, "Int") ""
	datafile .= SubStr(lastPosition, 1, -1) ""
	
	;~ FileSetAttrib, -RH, % Config.SaveFile
	FileOpen(Config.SaveFile, "w").Write(datafile)
	;~ FileSetAttrib, +RH, % Config.SaveFile
}

GuiShow()
{
	Gui, % gHwnd.GUI ":Show", Restore
}
GuiClose()
{
	if (config.ExitOnClose)
		ExitApp
	Gui, Hide
}
GuiContextMenu()
{
	return
}
GuiSize(hwnd, event, width, height)
{
	GuiControlGet, tab, Pos, % gHwnd.Tab
	w := width - tabx - 10, h := height - taby - 10
	if (w < 1 || h < 1)
		return
	GuiControl, Move, % gHwnd.Tab, w%w% h%h%
	for k, v in ListBox
	{
		GuiControlGet, list, Pos, % v.Hwnd
		h := height - listy - 25, h := h < 1 ? 1 : h
		GuiControl, Move, % v.Hwnd, h%h%
	}
	GuiControlGet, edit, Pos, % gHwnd.ConfigEdit
	GuiControlGet, save, Pos, % gHwnd.ConfigSave
	w := width - editx - 25 - savew, h := height - edity - 25
	if (w > 200 && h > 1)
	{
		GuiControl, Move, % gHwnd.ConfigEdit, w%w% h%h%
		if ((x := w + 20) > 1)
			GuiControl, Move, % gHwnd.ConfigSave, x%x%
	}
}

Initialize()
{
	ParseConfiguration()
	
	xMain := 10, yMain := 15, wMain := 140, hMain := 175
	
	xTab := wMain+xMain*2, yTab := yMain
	
	xList := xTab, yList := yTab+35, wList := 150, hList := 185, ListPad := 15

	Menu, Tray, NoStandard
	Menu, Tray, Add, Show, GuiShow
	Menu, Tray, Default, Show
	Menu, Tray, Add
	Menu, Tray, Add, Exit, Exit
	
	minSize := (wMain+xMain*2) "x" (hMain+yMain*2)
	Gui, New, +Resize +MinSize%minSize% +hwndmain +Delimiter, NegativeScreen
	gHwnd.GUI := main
	CreateMainControls(xMain, yMain, wMain, hMain)
	BrightnessSlider_Create(xMain, yMain*2+hMain, wMain, 200)
	Gui, Add, Tab3, x%xTab% y%yMain% hwndh w500 h200 gTab_Sub, % "SelectCreateEdit File"
	gHwnd.Tab := h
	ListBox_Create(xList, yList, wList, hList, ListPad)
	Gui, Tab, 2
	EditField_Create(xList+10, yList)
	Gui, Tab, 3
	Gui, Add, Edit, % "x" xList+15 " y" yList " h" hList+yMain " w450 gEditConfigFile hwndh", % FileOpen(Config.ConfigFile, "r").Read()
	gHwnd.ConfigEdit := h
	Gui, Add, Button, X+5 Yp H30 W45 Disabled gSaveConfigFile hwndh, Save
	gHwnd.ConfigSave := h
	Gui, Show, Hide
	if (!LoadSavedState())
		Gui, Show
	OnExit("SaveCurrentState")

	ColorEffect.fadeToggle := Config.SmoothToggles
	ColorEffect.fadeTrans := Config.SmoothTransitions
	ColorEffect.CurrentMatrix := CreateMatrixFromSelection()
	if (Config.ActiveOnStartup)
		ApplySelection()
	try Hotkey, % config.Exit, Exit
	try Hotkey, % config.Toggle, ToggleColorEffect
}

CreateMainControls(x, y, w, h)
{
	Gui, Add, GroupBox, X%x% Y%y% W%w% H%h%, Buttons
	Gui, Font, S9
	DllCall("SystemParametersInfo", "Int", 0x5E, "Int", 0, "Int*", s, "Int", 0)
	Gui, Add, Checkbox, Xp+10 Yp+15 Wp-20 H30 gInvertMouse Checked%s%, Affect cursor color
	s := Config.SmoothToggles || Config.SmoothTransitions
	Gui, Add, Checkbox, Xp+0 Yp+25 Wp Hp Checked%s% Disabled, Smooth Transition
	Gui, Add, Checkbox, Xp+0 Yp+25 Wp Hp hwndh gInvertSelection, Invert Selection
	gHwnd.CheckboxInvert := h
	Gui, Add, Button, Xp Y+0 Wp Hp Default hwndh gToggleColorEffect, Enable Effect
	gHwnd.ButtonToggle := h
	ColorEffect.NotifyFunction := "ColorEffectChanged"
	Gui, Add, Button, Xp Y+0 Wp Hp hwndh gApplySelection, Apply Selection
	gHwnd.ButtonApply := h
}

EditField_Create(x, y)
{
	EditField.Name := ["Red", "Green", "Blue", "Temp", "Hue", "Sat", "Lum", "Con", "Swap Red Green", "Swap Green Blue", "Swap Blue Red", "Matrix"]
	EditField.DefaultContent := [255, 255, 255, 6600, "+0", "+0", "+0", "+0", false, false, false, ""]
	EditField.Content := [255, 255, 255, 6600, "+0", "+0", "+0", "+0", false, false, false, ""]
	Gui, Font, S10
	Gui, Add, Groupbox, X%x% Y%y% W300 H55, Intensity
	Gui, Add, Text, Section Xp+10 Yp+20 W0 H0
	fieldIndex := 0
	Loop, 3
	{
		++fieldIndex
		Gui, Add, Text, X+0 Yp+3 W40 R1 Center, % EditField.Name[fieldIndex]
		Gui, Add, Edit, X+0 Yp-3 W50 R1 hwndh gEditField_Sub
		SendMessage, 0x1501, 0, % &v:=EditField.Content[fieldIndex],, ahk_id %h%
		EditField.Hwnd.Push(h)
	}
	Gui, Add, Groupbox, Xs-10 Y+25 W480 H55, Offset
	Gui, Add, Text, Xs Yp+20 W0 H0
	Loop, 5
	{
		++fieldIndex
		Gui, Add, Text, X+0 Yp+3 W40 R1 Center, % EditField.Name[fieldIndex]
		Gui, Add, Edit, X+0 Yp-3 W50 R1 hwndh gEditField_Sub
		SendMessage, 0x1501, 0, % &v:=EditField.Content[fieldIndex],, ahk_id %h%
		EditField.Hwnd.Push(h)
	}
	Gui, Add, Text, Xs+320 Ys-12 W0 H0
	Loop, 3
	{
		++fieldIndex
		Gui, Add, Checkbox, Xp Y+0 R1 hwndh gEditField_Sub, % EditField.Name[fieldIndex]
		EditField.Hwnd.Push(h)
	}
	Gui, Add, Button, X%x% Y+75 W80 H30 gEditField_Reset, Reset
	Gui, Add, Edit, X%x% Y+5 W480 R5 -VScroll +HScroll -Wrap hwndh
	EditField.Hwnd.Push(h)
}
EditField_Reset()
{
	for k, v in EditField.Hwnd
	{
		WinGetClass, wclass, ahk_id %v%
		if (wclass == "Edit")
			GuiControl,, %v%
		else
			GuiControl,, %v%, 0
		EditField.Content[k] := EditField.DefaultContent[k]
	}
}
EditField_Sub(h)
{
	static hEdit
	for k, v in EditField.Hwnd
		if (!hEdit && EditField.Name[k] = "Matrix")
			hEdit := v
		else if (v = h) && hEdit
			break
		
	GuiControlGet, buf,, %h%
	if buf is number
		EditField.Content[k] := buf
	else
		EditField.Content[k] := EditField.DefaultContent[k]
	matrix := EditField_CreateColorMatrix(EditField.Content*)
	GuiControlGet, invert,, % gHwnd.CheckboxInvert
	if (invert)
		matrix := ColorMatrix.Invert(matrix)
	GuiControl,, %hEdit%, % ColorMatrix.Format(matrix)
}
EditField_Set(content*)
{
	for k, v in content
	{
		if v is number
			EditField.Content[k] := v
		GuiControl,, % EditField.Hwnd[k], % EditField.Content[k]
	}
	for k, v in EditField.Name
		if (v = "Matrix")
			GuiControl,, % EditField.Hwnd[k], % ColorMatrix.Format(EditField_CreateColorMatrix(EditField.Content*))
}

EditField_CreateColorMatrix(Red, Green, Blue, Temp, Hue, Sat, Lum, Con, Flag1, Flag2, Flag3)
{
	M := ColorMatrix.IdentityMatrix
	; SwapRG, SwapGB, SwapBR
	Flags := [Flag1, Flag2, Flag3]
	
	; RGB perceptive luminance constants
	; LumR := 0.299, LumG := 0.587, LumB := 0.114
	; LumR := 0.3086, LumG := 0.6094, LumB := 0.0820
	LumR := 0.2126005530748145516, LumG := 0.7151846259926298499, LumB := 0.07221482093255522685
	
	; hue rotation
	Hue := Mod(Hue, 360) / 180 * ACos(-1), cosHue := Cos(Hue)
	rcVector := (1 - cosHue) * LumR, gcVector := (1 - cosHue) * LumG, bcVector := (1 - cosHue) * LumB
	rxVector := Sin(Hue) * Sqrt(LumR), gxVector := Sin(Hue) * Sqrt(LumG), bxVector := Sin(Hue) * Sqrt(LumB)
	
	Mx := [ [rcVector + cosHue, rcVector + gxVector, rcVector - bxVector, 0, 0]
		  , [gcVector - rxVector, gcVector + cosHue, gcVector + bxVector, 0, 0]
		  , [bcVector + rxVector, bcVector - gxVector, bcVector + cosHue, 0, 0]
		  , [0, 0, 0, 1, 0]
		  , [0, 0, 0, 0, 1] ]
	M := ColorMatrix.Multiply(M, Mx)
	
	; saturation offset
	Sat := (Sat < -100 ? -100 : Sat > 100 ? 100 : Sat) / 100 * -((Sat < 0) + 3 * (Sat > 0))
	rSat := LumR * Sat, gSat := LumG * Sat, bSat := LumB * Sat, dSat := 1 - Sat
	Mx := [ [rSat + dSat, rSat, rSat, 0, 0]
		  , [gSat, gSat + dSat, gSat, 0, 0]
		  , [bSat, bSat, bSat + dSat, 0, 0]
		  , [0, 0, 0, 1, 0]
		  , [0, 0, 0, 0, 1] ]
	M := ColorMatrix.Multiply(M, Mx)
	
	; luminance offset
	Lum := (Lum < -100 ? -100 : Lum > 100 ? 100 : Lum) / 100
	Mx := [ [0, 0, 0, 0, 0]
		  , [0, 0, 0, 0, 0]
		  , [0, 0, 0, 0, 0]
		  , [0, 0, 0, 0, 0]
		  , [Lum, Lum, Lum, 0, 0] ]
	M := ColorMatrix.Add(M, Mx)
	
	; contrast offset
	Con := (Con < -100 ? -100 : Con > 100 ? 100 : Con) / 100
	Mx := [ [Con + 1, 0, 0, 0, 0]
		  , [0, Con + 1, 0, 0, 0]
		  , [0, 0, Con + 1, 0, 0]
		  , [0, 0, 0, 1, 0]
		  , [Con / 2, Con / 2, Con / 2, 0, 1] ]
	M := ColorMatrix.Multiply(M, Mx)
	
	; color temperature in kelvin from 1000K to 100000K
	; http://www.tannerhelland.com/4435/convert-temperature-rgb-algorithm-code/
	Temp := (Temp < 1000 ? 1000 : Temp > 100000 ? 100000 : Temp) / 100
	TempR := Temp > 66.49 ? (0.0004478684462124118 * Temp - 0.15785750232675010 * Ln(Temp-55) + 1.3556688263134426) : 1
	TempG := Temp > 66.00 ? (0.0003115080994769546 * Temp - 0.11013841706194390 * Ln(Temp-50) + 1.2606968011877107)
						  : (-0.001748900018414868 * Temp + 0.40977318428995639 * Ln(Temp-02) - 0.5887713629425436)
	TempB := Temp > 18.51 ? (0.0032447435545127039 * Temp + 0.45364683925749596 * Ln(Temp-10) - 1.0315429329616330) * (Temp < 65.24) + (Temp >= 65.24) : 0
	Mx := [ [TempR, 0, 0, 0, 0]
		  , [0, TempG, 0, 0, 0]
		  , [0, 0, TempB, 0, 0]
		  , [0, 0, 0, 1, 0]
		  , [0, 0, 0, 0, 1] ]
	M := ColorMatrix.Multiply(M, Mx)
	
	; color component offset
	Mx := [ [(Abs(Red) < 255 ? Abs(Red) / 255 : 1), 0, 0, 0, 0]
		  , [0, (Abs(Green) < 255 ? Abs(Green) / 255 : 1), 0, 0, 0]
		  , [0, 0, (Abs(Blue) < 255 ? Abs(Blue) / 255 : 1), 0, 0 ]
		  , [0, 0, 0, 1, 0]
		  , [0, 0, 0, 0, 1] ]
	M := ColorMatrix.Multiply(M, Mx)
	
	; swap color
	;!! needs a better algorithm
	Loop, 3
	{
		if (Flags[A_Index])
		{
			x := A_Index, y := x < 3 ? x + 1 : 1
			v := M[x][x], M[x][x] := M[y][x], M[y][x] := v
			v := M[y][y], M[y][y] := M[x][y], M[x][y] := v
		}
	}
	return M
}

ListBox_Create(x, y, w, h, pad)
{
	Gui, Add, Text, X%x%
	for k, v in ListBox
	{
		Gui, Font, S9 Bold
		Gui, Add, Text, X+%pad% Y%y% w%w% Center hwndhText, % v.Name
		Gui, Font, S10 Normal
		Gui, Add, ListBox, Xp Yp+20 W%w% H%h% Multi 0x1100 hwndhCtrl, % v.Content
		v.Hwnd := hCtrl, v.TextHwnd := hText
	}
	oldProc := DllCall("GetWindowLong"(A_PtrSize != 8 ?: "Ptr"), "Ptr", hCtrl, "Int", -4, "Ptr")
	newProc := RegisterCallback("ListBox_WndProc",, 4, oldProc)
	for k, v in ListBox
		DllCall("SetWindowLong"(A_PtrSize != 8 ?: "Ptr"), "Ptr", v.Hwnd, "Int", -4, "Ptr", newProc)
}
ListBox_GetSelection()
{
	selection := []
	for k, v in ListBox
	{
		GuiControlGet, item,, % v.Hwnd
		selection[k] := StrSplit(item, "")
	}
	return selection
}
ListBox_SetSelection(sel)
{
	for index, list in sel
		for k, v in list
			if (pos := DllCall("SendMessage", "Ptr", ListBox[index].Hwnd, "Int", 0x1A2, "Ptr", 0, "Ptr", &v) + 1)
				GuiControl, Choose, % ListBox[index].Hwnd, %pos%
}
ListBox_WndProc(hwnd, msg, wParam, lParam)
{
	Critical 1000
	if (msg = 0x203) || (msg = 0x101 && wParam = 32) ; WM_LBUTTONDBLCLK || WM_KEYUP && VK_SPACE
	{
		TailCall("ApplySelection")
	}
	else if (msg = 0x100) && (wParam = 0x25 || wParam = 0x27) ; WM_KEYDOWN && (VK_LEFT || VK_RIGHT)
	{
		for i, ctrl in ListBox
			continue
		until (ctrl.Hwnd = hwnd)
		m := ListBox.Length()
		ctrl := ListBox[wParam = 0x27 ? m > i ? i + 1 : m : i > 1 ? i - 1 : 1].Hwnd ; one way scroll
		; ctrl := ListBox[wParam = 0x27 ? m > i ? i + 1 : 1 : i > 1 ? i - 1 : m].Hwnd ; circular scroll
		curSel       := DllCall(A_EventInfo, "Ptr", hwnd, "Int", 0x188, "Ptr", 0, "Ptr", 0)
		curTopIndex  := DllCall(A_EventInfo, "Ptr", hwnd, "Int", 0x18E, "Ptr", 0, "Ptr", 0)
		ctrlMaxIndex := DllCall(A_EventInfo, "Ptr", ctrl, "Int", 0x18B, "Ptr", 0, "Ptr", 0)
		ctrlTopIndex := DllCall(A_EventInfo, "Ptr", ctrl, "Int", 0x18E, "Ptr", 0, "Ptr", 0)
		thisSel      := curSel - curTopIndex + ctrlTopIndex + 1
		GuiControl, Choose, %ctrl%, % thisSel < ctrlMaxIndex ? thisSel : ctrlMaxIndex
		GuiControl, Choose, % ctrl != hwnd ? hwnd : 0, 0
		GuiControl, Focus, %ctrl%
		return 0
	}
	else if (msg = 0x7 || msg = 0x201) && (GetKeyState("Control", "P") = 0) ; (WM_SETFOCUS || WM_LBUTTONDOWN) && ctrl is not pressed
	{
		for k, v in ListBox
			if (v.Hwnd != hwnd)
				GuiControl, Choose, % v.Hwnd, 0
	}
	return DllCall(A_EventInfo, "Ptr", hwnd, "Int", msg, "Ptr", wParam, "Ptr", lParam)
}

BrightnessSlider_Create(x, y, w, h)
{
	Gui, Add, GroupBox, X%x% Y%y% W%w% H100, Brightness
	lv := Brightness.Get("Levels") - 1
	Gui, Add, Slider, Xp+1 Yp+20 Wp-2 Range0-%lv% NoTicks AltSubmit Buddy2 gBrightnessSlider_Sub hwndh
	gHwnd.Slider := h
	Gui, Add, Button, Xp+10 Y+5 Wp-20 H30 gTurnOffScreen, Turn Off Screen
	Brightness.NotifyFunction := Func("BrightnessSlider_Update")
	BrightnessSlider_Update()
}
BrightnessSlider_Sub(h)
{
	if (A_GuiEvent = "Normal")
		return
	GuiControlGet, cur,, %h%
	Brightness.Set(cur, false)
	if (A_GuiEvent = 5)
	{
		CoordMode, Tooltip, Client
		GuiControlGet, pos, Pos, %h%
		lv := Brightness.Get("Levels")
		ToolTip, % Brightness.Get(cur), % posx + posw/lv*cur, % posy + posh
	}
	else ToolTip
}
BrightnessSlider_Update()
{
	GuiControl,, % gHwnd.Slider, % Brightness.Get()
}

Tab_Sub(h)
{
	static sel
	GuiControlGet, cur,, %h%
	if (cur != "Select")
		sel := ListBox_GetSelection()
	else
		ListBox_SetSelection(sel)
}
CreateMatrixFromSelection()
{
	GuiControlGet, state,, % gHwnd.Tab
	if (state = "Select")
	{
		result := ColorMatrix.IdentityMatrix
		for index, list in ListBox_GetSelection()
			for k, name in list
				result := ColorMatrix.Multiply(result, ListBox[index].Matrix[name])
	}
	else if (state = "Create")
		result := EditField_CreateColorMatrix(EditField.Content*)
	else return
		
	GuiControlGet, state,, % gHwnd.CheckboxInvert
	if (state)
		result := ColorMatrix.Invert(result)
	return result
}
ApplySelection()
{
	GuiControl, Disable, % gHwnd.ButtonApply
	ColorEffect.SetEffect(CreateMatrixFromSelection())
	GuiControl, Enable, % gHwnd.ButtonApply
}
ToggleColorEffect()
{
	GuiControl, Disable, % gHwnd.ButtonToggle
	ColorEffect.Toggle()
	GuiControl, Enable, % gHwnd.ButtonToggle
}
ColorEffectChanged()
{
	GuiControl,, % gHwnd.ButtonToggle, % ColorEffect.IsEnabled() ? "Disable Effect" : "Enable Effect"
}
TurnOffScreen()
{
	DllCall("DefWindowProc", "Ptr", A_ScriptHwnd, "Int", 0x112, "Ptr", 0xF170, "Ptr", 2)
}
InvertMouse(h)
{
	GuiControlGet, invert,, %h%
	DllCall("SystemParametersInfo", "Int", 0x5D, "Int", -invert, "Ptr", 0, "Int", 0)
}
InvertSelection(h)
{
	GuiControlGet, lastTab,, % gHwnd.Tab
	if (lastTab = "Create")
		EditField_Sub(0)
}
EditConfigFile(h)
{
	GuiControl, Enable, % gHwnd.ConfigSave
}
SaveConfigFile(h)
{
	GuiControl, Disable, % gHwnd.ConfigSave
	GuiControlGet, data,, % gHwnd.ConfigEdit
	FileOpen(config.ConfigFile, "w").Write(data)
}
Exit()
{
	ExitApp
}

class ColorMatrix
{
	__Get(p)
	{
		if (p = "ZeroMatrix")
			return [[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,0,0,0]]
		if (p = "IdentityMatrix")
			return [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0],[0,0,0,1,0],[0,0,0,0,1]]
		if (p = "OneMatrix")
			return [[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1],[1,1,1,1,1]]
		if (p = "NegateMatrix")
			return [[-1,0,0,0,0],[0,-1,0,0,0],[0,0,-1,0,0],[0,0,0,-1,0],[0,0,0,0,-1]]
	}
	__Set(p, v)
	{
		if v := this.__Get(p)
			return v
	}
	class Iterator
	{
		_NewEnum()
		{
			return that := new this, that.x := 1, that.y := 1
		}
		Next(ByRef x, ByRef y)
		{
			return (x := this.x) <= 5, (y := this.y++) < 5 || (this.x++, this.y := 1)
		}
	}
	IsValid(m)
	{
		for x, y in this.Iterator
			if !(m[x][y] | 1)
				return false
		return true
	}
	IsEqual(a, b)
	{
		if (!this.IsValid(a) || !this.IsValid(b))
			return
			
		for x, y in this.Iterator
			if (a[x][y] != b[x][y])
				return false
		return true
	}
	Clone(m)
	{
		if (!this.IsValid(m))
			return
		
		r := this.ZeroMatrix
		for x, y in this.Iterator
			r[x][y] := m[x][y] + 0
		return r
	}
	Add(a, b) ; b can be a number for translation
	{
		if (!this.IsValid(a) || !(this.IsValid(b) || scalar := b | 1))
			return
		
		r := this.ZeroMatrix
		for x, y in this.Iterator
			r[x][y] := a[x][y] + (scalar ? b : b[x][y])
		return r
	}
	; b can be a number for scaling, pass true as last parameter to perform element wise multiplication
	Multiply(a, b, hadamard := false)
	{
		if (!this.IsValid(a) || !(this.IsValid(b) || scalar := b | 1))
			return
		
		r := this.ZeroMatrix
		if scalar or hadamard
			for x, y in this.Iterator
				r[x][y] := a[x][y] * (scalar ? b : b[x][y])
		else for x, y in this.Iterator
			Loop, 5
				z := A_Index, r[x][y] += a[x][z] * b[z][y]
		return r
	}
	Invert(m) ; inverse color effect, not an inverted matrix of M^-1
	{
		if (!this.IsValid(m))
			return
		
		r := this.Clone(m)
		for x, y in this.Iterator
			(x > 3 || y > 3 || r[5][y] -= r[x][y] := -m[x][y])
		return r
	}
	Interpolate(from, to, tpoint)
	{
		if (!this.IsValid(from) || !this.IsValid(to))
			return
			
		r := [], delta := this.Add(to, this.Multiply(from, -1))
		Loop, % tpoint := tpoint < 1 ? 1 : Round(tpoint)
			t := A_Index, r[t] := this.Add(from, this.Multiply(delta, t / tpoint))
		return r
	}
	Format(m, f := "")
	{
		f ~= "^(?:\d*\.\d*|\d+)[eE]?$" || f := A_FormatFloat, f := " {:" (f | 1 ? f "f" : f) "}"
		for k, v in this.Clone(m)
			r .= Format("{}{{}" f "," f "," f "," f "," f " {}}", k > 1 ? "`r`n" : "", v*)
		return r
	}
}

class ColorEffect
{
	notifyFunction := "" ; function name or object to be called when color effect changed
	fadeToggle := true
	fadeTrans := true
	currentMatrix := 0
	
	__New()
	{
		try
		{
			DllCall("dwmapi\DwmIsCompositionEnabled", "Int*", cEnabled)
			if (!cEnabled)
				throw Exception("DWM Composition is disabled", "ColorEffect")
			if !(hMag := DllCall("LoadLibrary", "Str", "magnification", "Ptr"))
				throw Exception("Failed to load magnification.dll (Error " A_LastError ")", "ColorEffect")
			hUser := DllCall("GetModuleHandle", "Str", "user32", "Ptr")
			
			if !(this.__GetEffect := DllCall("GetProcAddress", "Ptr", hMag, "AStr", "MagGetFullscreenColorEffect", "Ptr")) ; windows 8 and up
			if !(this.__GetEffect := DllCall("GetProcAddress", "Ptr", hUser, "AStr", "GetMagnificationDesktopColorEffect", "Ptr"))
				throw Exception("Fullscreen color effect API not found (Error " A_LastError ")", "ColorEffect")
			
			if !(this.__SetEffect := DllCall("GetProcAddress", "Ptr", hMag, "AStr", "MagSetFullscreenColorEffect", "Ptr")) ; windows 8 and up
			if !(this.__SetEffect := DllCall("GetProcAddress", "Ptr", hUser, "AStr", "SetMagnificationDesktopColorEffect", "Ptr"))
				throw Exception("Fullscreen color effect API not found (Error " A_LastError ")", "ColorEffect")
			
			if !(DllCall("magnification\MagInitialize"))
				throw Exception("Magnifier initialization failed (Error " A_LastError ")", "ColorEffect")
			this.MagnifierAPIhandle := hMag
		}
		catch e
			ErrorLevel := e.Message
	}
	__Delete()
	{
		this.Disable()
		DllCall("magnification\MagUninitialize")
		DllCall("FreeLibrary", "Ptr", this.MagnifierAPIhandle)
	}
	GetEffect()
	{
		VarSetCapacity(cm, 4*5*5, 0)
		DllCall(this.__GetEffect, "Ptr", &cm)
		m := ColorMatrix.ZeroMatrix
		for x, y in ColorMatrix.Iterator
			m[x][y] := NumGet(cm, (x-1)*4*5 + (y-1)*4, "Float")
		return m
	}
	SetEffect(m, fade := "", fadeSteps := 20)
	{
		if (ColorMatrix.IsEqual(this.GetEffect(), m) || !ColorMatrix.IsValid(m))
			return
		
		fade := fade = "" && (this.fadeTrans || this.fadeToggle) || fade
		for k, v in ColorMatrix.Interpolate(this.GetEffect(), m, fade ? fadeSteps : 1)
		{
			VarSetCapacity(cm, 4*5*5, 0)
			for x, y in ColorMatrix.Iterator
				NumPut(v[x][y], cm, (x-1)*4*5 + (y-1)*4, "Float")
			if (!(r := DllCall(this.__SetEffect, "Ptr", &cm)) || !fade)
				break
			Sleep, 1
		}
		return r, this.IsEnabled() && this.CurrentMatrix := ColorMatrix.Clone(m), TailCall(this.NotifyFunction)
	}
	IsEnabled()
	{
		return !ColorMatrix.IsEqual(this.GetEffect(), ColorMatrix.IdentityMatrix)
	}
	Enable()
	{
		this.SetEffect(this.CurrentMatrix)
	}
	Disable()
	{
		this.SetEffect(ColorMatrix.IdentityMatrix)
	}
	Toggle()
	{
		this.IsEnabled() ? this.Disable() : this.Enable()
	}
}

class Brightness
{
	notifyFunction := "" ; function name or object to be called when brightness changed
	__New()
	{
		sink := ComObjCreate("WbemScripting.SWbemSink")
		WMI := ComObjGet("winmgmts:ROOT\WMI"), ComObjConnect(sink, this)
		WMI.ExecNotificationQueryAsync(sink, "SELECT * FROM WmiMonitorBrightnessEvent WHERE Active=TRUE")
		WMI.ExecQuery("SELECT * FROM WmiMonitorBrightnessMethods WHERE Active=TRUE")._NewEnum()[monMethods]
		WMI.ExecQuery("SELECT * FROM WmiMonitorBrightness WHERE Active=TRUE")._NewEnum()[monitor]
		this.monMethods := monMethods, this.monitor := monitor, this.sink := sink, this.currentLevel := 0
		this.OnObjectReady({"Brightness":this.monitor.CurrentBrightness})
	}
	Get(arg := "Current")
	{
		try return arg = "Current" ? this.currentLevel : arg = "Levels" ? this.monitor.levels : this.monitor.level[arg]
	}
	Set(level, relative := true)
	{
		try return this.monMethods.WmiSetBrightness(0, this.monitor.level[this.currentLevel * (!!relative) + level]) or true
	}
	OnObjectReady(obj)
	{
		for n in this.monitor.level
			if (n = obj.Brightness)
				this.currentLevel := A_Index-1
		TailCall(this.NotifyFunction)
	}
}

; helper functions
StrConcat(d := "", s*)
{
	for s, s in s
		r .= d . s
	return SubStr(r, 2)
}
TailCall(f, p*)
{
	if !(IsFunc(f) && f := IsObject(f) ? f : Func(f))
		return
	Loop, % f.MaxParams - p.Length()
		p.Push("")
	f := f.Bind(p*)
	SetTimer, %f%, -16
}
