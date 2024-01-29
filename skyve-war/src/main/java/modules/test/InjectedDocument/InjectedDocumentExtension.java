package modules.test.InjectedDocument;

import java.util.Map;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.inject.Inject;
import modules.test.domain.InjectedDocument;

public class InjectedDocumentExtension extends InjectedDocument {
	private static final long serialVersionUID = 8559142836072940662L;

	@Inject
	public Persistence p;
	@Inject
	public Customer c;
	@Inject
	public User u;
	@Inject
	public Map<String, Object> s;
	@Inject
	public Repository r;
}
