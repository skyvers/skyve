package modules.admin.Communication;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Communication.actions.CreateFiles;
import modules.admin.Communication.actions.GetCount;
import modules.admin.Communication.actions.GetResults;
import modules.admin.Communication.actions.SendNow;
import modules.admin.Communication.actions.TestSend;
import modules.admin.domain.AdminDocument;
import modules.admin.domain.Contact;

/**
 * Creates Communication documents with default batching and scheduling values.
 */
@SkyveFactory(testDomain = false, excludedActions = {
		CreateFiles.class, GetCount.class, GetResults.class, SendNow.class, TestSend.class })
public class CommunicationFactory {
	/**
	 * Performs the crudInstance operation.
	 * @return the operation result
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static CommunicationExtension crudInstance() {
		CommunicationExtension bean = new DataBuilder().factoryBuild(AdminDocument.COMMUNICATION);
		bean.setSystemUse(Boolean.FALSE);
		bean.setTag(new DataBuilder().build(AdminDocument.TAG));

		return bean;
	}

	// Ensure that system is switched off so that the communication can be deleted by the SAIL test.
	/**
	 * Performs the sail operation.
	 * @return the operation result
	 */
	@SuppressWarnings("static-method")
	@SkyveFixture(types = FixtureType.sail)
	public CommunicationExtension sail() {
		CommunicationExtension result = new DataBuilder().factoryBuild(AdminDocument.COMMUNICATION);
		result.setSystemUse(Boolean.FALSE);
		result.setCalendarStartTime(null);
		result.setCalendarEndTime(null);
		result.setModuleName(Contact.MODULE_NAME);
		result.setDocumentName(Contact.DOCUMENT_NAME);
		return result;
	}
}
