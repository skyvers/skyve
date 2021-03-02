package modules.admin.DocumentNumber;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Contact;
import modules.admin.domain.DocumentNumber;

public class DocumentNumberFactory {

	@SkyveFixture(types = FixtureType.crud)
	public static DocumentNumber crudInstance() {
		DocumentNumber dn = new DataBuilder().build(DocumentNumber.MODULE_NAME, DocumentNumber.DOCUMENT_NAME);
		dn.setModuleName(Contact.MODULE_NAME);
		dn.setDocumentName(Contact.DOCUMENT_NAME);

		return dn;
	}

}
