package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.impl.generate.sail.Generator;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

/**
 * Generates SAIL automation for menu navigation scenarios.
 */
public class GenerateMenuSAIL extends GenerateSAIL {
	/**
	 * Performs the single operation.
	 * @param user the user value
	 * @param loginCustomer the loginCustomer value
	 * @param loginPassword the loginPassword value
	 * @param moduleName the moduleName value
	 * @param uxui the uxui value
	 * @param userAgentType the userAgentType value
	 * @param testStrategy the testStrategy value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
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

	/**
	 * Performs the multiple operation.
	 * @param user the user value
	 * @param loginCustomer the loginCustomer value
	 * @param loginPassword the loginPassword value
	 * @param uxui the uxui value
	 * @param userAgentType the userAgentType value
	 * @param testStrategy the testStrategy value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
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
