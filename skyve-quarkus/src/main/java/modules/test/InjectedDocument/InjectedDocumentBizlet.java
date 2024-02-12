package modules.test.InjectedDocument;

import java.util.Map;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import jakarta.inject.Inject;

public class InjectedDocumentBizlet extends Bizlet<InjectedDocumentExtension> {
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
