FUNCTION Names: PNameRec;
  VAR HNames: PNameRec;
  BEGIN
    HNames := Nil;
    HNames := 
      NewNameRec(Ofs(TypeOf(TOBJECT)^), SizeOf(TOBJECT), 'TOBJECT',
      NewNameRec(Ofs(TypeOf(TNAMEDVIEW)^), SizeOf(TNAMEDVIEW), 'TNAMEDVIEW',
      NewNameRec(Ofs(TypeOf(TSTR)^), SizeOf(TSTR), 'TSTR',
      NewNameRec(Ofs(TypeOf(TINDEXFILE)^), SizeOf(TINDEXFILE), 'TINDEXFILE',
      NewNameRec(Ofs(TypeOf(TVIEW)^), SizeOf(TVIEW), 'TVIEW',
      NewNameRec(Ofs(TypeOf(TINPUTLINE)^), SizeOf(TINPUTLINE), 'TINPUTLINE',
      NewNameRec(Ofs(TypeOf(TSTRINPUTLINE)^), SizeOf(TSTRINPUTLINE), 'TSTRINPUTLINE',
      NewNameRec(Ofs(TypeOf(TSTATICTEXT)^), SizeOf(TSTATICTEXT), 'TSTATICTEXT',
      NewNameRec(Ofs(TypeOf(TPARAMTEXT)^), SizeOf(TPARAMTEXT), 'TPARAMTEXT',
      NewNameRec(Ofs(TypeOf(TLABEL)^), SizeOf(TLABEL), 'TLABEL',
      NewNameRec(Ofs(TypeOf(TBUTTON)^), SizeOf(TBUTTON), 'TBUTTON',
      NewNameRec(Ofs(TypeOf(TCLUSTER)^), SizeOf(TCLUSTER), 'TCLUSTER',
      NewNameRec(Ofs(TypeOf(TRADIOBUTTONS)^), SizeOf(TRADIOBUTTONS), 'TRADIOBUTTONS',
      NewNameRec(Ofs(TypeOf(TCHECKBOXES)^), SizeOf(TCHECKBOXES), 'TCHECKBOXES',
      NewNameRec(Ofs(TypeOf(THISTORY)^), SizeOf(THISTORY), 'THISTORY',
      NewNameRec(Ofs(TypeOf(TBACKGROUND)^), SizeOf(TBACKGROUND), 'TBACKGROUND',
      NewNameRec(Ofs(TypeOf(TGROUP)^), SizeOf(TGROUP), 'TGROUP',
      NewNameRec(Ofs(TypeOf(TDESKTOP)^), SizeOf(TDESKTOP), 'TDESKTOP',
      NewNameRec(Ofs(TypeOf(TPROGRAM)^), SizeOf(TPROGRAM), 'TPROGRAM',
      NewNameRec(Ofs(TypeOf(TAPPLICATION)^), SizeOf(TAPPLICATION), 'TAPPLICATION',
      NewNameRec(Ofs(TypeOf(TMAIN)^), SizeOf(TMAIN), 'TMAIN',
      NewNameRec(Ofs(TypeOf(TWINDOW)^), SizeOf(TWINDOW), 'TWINDOW',
      NewNameRec(Ofs(TypeOf(THISTORYWINDOW)^), SizeOf(THISTORYWINDOW), 'THISTORYWINDOW',
    HNames)))))))))))))))))))))));
    HNames :=
      NewNameRec(Ofs(TypeOf(TDIALOG)^), SizeOf(TDIALOG), 'TDIALOG',
      NewNameRec(Ofs(TypeOf(TBASEDIALOG)^), SizeOf(TBASEDIALOG), 'TBASEDIALOG',
      NewNameRec(Ofs(TypeOf(TUSEDIALOG)^), SizeOf(TUSEDIALOG), 'TUSEDIALOG',
      NewNameRec(Ofs(TypeOf(TSCROLLBAR)^), SizeOf(TSCROLLBAR), 'TSCROLLBAR',
      NewNameRec(Ofs(TypeOf(TLISTVIEWER)^), SizeOf(TLISTVIEWER), 'TLISTVIEWER',
      NewNameRec(Ofs(TypeOf(THISTORYVIEWER)^), SizeOf(THISTORYVIEWER), 'THISTORYVIEWER',
      NewNameRec(Ofs(TypeOf(TLISTBOX)^), SizeOf(TLISTBOX), 'TLISTBOX',
      NewNameRec(Ofs(TypeOf(TSTRLIST)^), SizeOf(TSTRLIST), 'TSTRLIST',
      NewNameRec(Ofs(TypeOf(TFRAME)^), SizeOf(TFRAME), 'TFRAME',
      NewNameRec(Ofs(TypeOf(TSCROLLER)^), SizeOf(TSCROLLER), 'TSCROLLER',
      NewNameRec(Ofs(TypeOf(TSTREAM)^), SizeOf(TSTREAM), 'TSTREAM',
      NewNameRec(Ofs(TypeOf(TNULLSTREAM)^), SizeOf(TNULLSTREAM), 'TNULLSTREAM',
      NewNameRec(Ofs(TypeOf(TEMSSTREAM)^), SizeOf(TEMSSTREAM), 'TEMSSTREAM',
      NewNameRec(Ofs(TypeOf(TDOSSTREAM)^), SizeOf(TDOSSTREAM), 'TDOSSTREAM',
      NewNameRec(Ofs(TypeOf(TBUFSTREAM)^), SizeOf(TBUFSTREAM), 'TBUFSTREAM',
      NewNameRec(Ofs(TypeOf(TRESOURCEFILE)^), SizeOf(TRESOURCEFILE), 'TRESOURCEFILE',
      NewNameRec(Ofs(TypeOf(TSTRINGLIST)^), SizeOf(TSTRINGLIST), 'TSTRINGLIST',
      NewNameRec(Ofs(TypeOf(TSTRLISTMAKER)^), SizeOf(TSTRLISTMAKER), 'TSTRLISTMAKER',
      NewNameRec(Ofs(TypeOf(TCOLLECTION)^), SizeOf(TCOLLECTION), 'TCOLLECTION',
      NewNameRec(Ofs(TypeOf(TSORTEDCOLLECTION)^), SizeOf(TSORTEDCOLLECTION), 'TSORTEDCOLLECTION',
      NewNameRec(Ofs(TypeOf(TVIEWCOLLECTION)^), SizeOf(TVIEWCOLLECTION), 'TVIEWCOLLECTION',
      NewNameRec(Ofs(TypeOf(TSTRCOLLECTION)^), SizeOf(TSTRCOLLECTION), 'TSTRCOLLECTION',
      NewNameRec(Ofs(TypeOf(TSTRINGCOLLECTION)^), SizeOf(TSTRINGCOLLECTION), 'TSTRINGCOLLECTION',
    HNames)))))))))))))))))))))));
    HNames :=
      NewNameRec(Ofs(TypeOf(TINDEXCOLLECTION)^), SizeOf(TINDEXCOLLECTION), 'TINDEXCOLLECTION',
      NewNameRec(Ofs(TypeOf(TRESOURCECOLLECTION)^), SizeOf(TRESOURCECOLLECTION), 'TRESOURCECOLLECTION',
    HNames));
   Names := HNames;
 END;
