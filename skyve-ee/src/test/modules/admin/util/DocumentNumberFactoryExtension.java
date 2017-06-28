package modules.admin.util;

import modules.admin.domain.DocumentNumber;

public class DocumentNumberFactoryExtension extends DocumentNumberFactory {

	@Override
	public DocumentNumber getInstance() throws Exception {
		DocumentNumber dn = super.getInstance();
		dn.setModuleName("admin");
		dn.setDocumentName("Contact");

		return dn;
	}

}
