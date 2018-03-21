package modules.admin.DocumentNumber;

import modules.admin.domain.DocumentNumber;
import modules.admin.util.DocumentNumberFactory;

public class DocumentNumberFactoryExtension extends DocumentNumberFactory {

	@Override
	public DocumentNumber getInstance() throws Exception {
		DocumentNumber dn = super.getInstance();
		dn.setModuleName("admin");
		dn.setDocumentName("Contact");

		return dn;
	}

}
