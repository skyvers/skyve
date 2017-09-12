package modules.test.InjectedDocument;

import java.sql.Connection;
import java.util.Map;

import javax.inject.Inject;

import org.skyve.content.ContentManager;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

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
	@Inject
	public Connection con;
	@Inject
	public ContentManager cm;
	@Inject
	public SQLDataAccess sda;
}
