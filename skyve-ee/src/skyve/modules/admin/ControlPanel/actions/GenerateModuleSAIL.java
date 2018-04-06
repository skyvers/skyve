package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.impl.generate.sail.Generator;
import org.skyve.impl.web.UserAgentType;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;

public class GenerateModuleSAIL extends GenerateSAIL {
	private static final long serialVersionUID = -2638179574509468527L;

	@Override
	protected Automation single(User user,
									String moduleName,
									String uxui,
									UserAgentType userAgentType,
									TestStrategy testStrategy)
	throws Exception {
		return Generator.visitModule(user, moduleName, uxui, userAgentType, testStrategy);
	}

	@Override
	protected List<Automation> multiple(User user,
											String uxui,
											UserAgentType userAgentType,
											TestStrategy testStrategy)
	throws Exception {
		return Generator.visitModules(user, uxui, userAgentType, testStrategy);
	}
}
