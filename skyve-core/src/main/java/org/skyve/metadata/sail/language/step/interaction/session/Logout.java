package org.skyve.metadata.sail.language.step.interaction.session;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Terminates the current user session, logging the automation user out of
 * the Skyve application.
 *
 * <p>Side effects: the web session is invalidated; any subsequent steps that
 * require authentication must be preceded by a new {@link Login} step.
 *
 * @see Login
 * @see org.skyve.metadata.sail.execution.Executor#executeLogout
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Logout implements Step {

	/**
	 * Executes execute.
	 * @param executor the executor
	 */
	@Override
	public void execute(Executor executor) {
		executor.executeLogout(this);
	}
	
	/**
	 * Returns the identifier.
	 * @param context the context
	 * @return the result
	 */
	@Override
	public String getIdentifier(AutomationContext context) {
		return null;
	}
}
