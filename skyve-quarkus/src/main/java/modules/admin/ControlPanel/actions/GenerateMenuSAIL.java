package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.impl.generate.sail.Generator;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

public class GenerateMenuSAIL extends GenerateSAIL {
	@Override
	public Automation single(User user,
								String loginCustomer,
								String loginPassword,
								String moduleName,
								String uxui,
								UserAgentType userAgentType,
								TestStrategy testStrategy)
	throws Exception {
		return Generator.visitMenu(user, loginCustomer, loginPassword, moduleName, uxui, userAgentType, testStrategy);
	}

	@Override
	public List<Automation> multiple(User user,
										String loginCustomer,
										String loginPassword,
										String uxui,
										UserAgentType userAgentType,
										TestStrategy testStrategy)
	throws Exception {
		return Generator.visitMenus(user, loginCustomer, loginPassword, uxui, userAgentType, testStrategy);
	}
}
