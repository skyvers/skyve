package modules.test.InjectedDocument;

import java.util.Map;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.inject.Inject;
import modules.test.domain.InjectedDocument;

/**
 * Provides a generated-document extension that verifies CDI/Jakarta injection points.
 */
public class InjectedDocumentExtension extends InjectedDocument {
	private static final long serialVersionUID = 8559142836072940662L;

	/**
	 * Injected persistence context.
	 */
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	public Persistence p;

	/**
	 * Injected customer context.
	 */
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	public Customer c;

	/**
	 * Injected user context.
	 */
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	public User u;

	/**
	 * Injected session-scoped map.
	 */
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	public Map<String, Object> s;
	
	/**
	 * Injected metadata repository.
	 */
	@Inject
	@SuppressWarnings("java:S6813") // allow member injection
	public Repository r;
}
