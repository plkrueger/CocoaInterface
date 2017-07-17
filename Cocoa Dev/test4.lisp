(setf dc (#/sharedDocumentController ns:ns-document-controller))

(#/defaultType dc)

(setf mm (#/mainMenu #&NSApp))

(setf mi (#/itemAtIndex: (#/submenu (#/itemWithTitle: mm #@"File")) 1))

(setf smh (iu::smh))

(iu::key-for-menuitem mi)


