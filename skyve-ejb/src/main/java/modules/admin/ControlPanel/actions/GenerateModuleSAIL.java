package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.impl.generate.sail.Generator;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

public class GenerateModuleSAIL extends GenerateSAIL {
	@Override
	protected Automation single(User user,
									String loginCustomer,
									String loginPassword,
									String moduleName,
									String uxui,
									UserAgentType userAgentType,
									TestStrategy testStrategy)
	throws Exception {
		return Generator.visitModule(user, loginCustomer, loginPassword, moduleName, uxui, userAgentType, testStrategy);
	}

	@Override
	protected List<Automation> multiple(User user,
											String loginCustomer,
											String loginPassword,
											String uxui,
											UserAgentType userAgentType,
											TestStrategy testStrategy)
	throws Exception {
		return Generator.visitModules(user, loginCustomer, loginPassword, uxui, userAgentType, testStrategy);
	}
}
