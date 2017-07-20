package modules.admin.util;

import org.skyve.util.test.SkyveFactory;

import modules.admin.Communication.actions.CreateFiles;
import modules.admin.Communication.actions.GetResults;
import modules.admin.Communication.actions.SendNow;
import modules.admin.Communication.actions.TestSend;
import modules.admin.domain.Communication;

@SkyveFactory(excludedActions = { CreateFiles.class, GetResults.class, SendNow.class, TestSend.class })
public class CommunicationFactoryExtension extends CommunicationFactory {

	@Override
	public Communication getInstance() throws Exception {
		Communication comm = super.getInstance();

		comm.setTag(new TagFactoryExtension().getInstance());
		comm.setSystem(Boolean.FALSE);

		return comm;
	}

}
