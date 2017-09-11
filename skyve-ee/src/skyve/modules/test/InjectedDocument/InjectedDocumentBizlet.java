package modules.test.InjectedDocument;

import java.sql.Connection;
import java.util.Map;

import javax.inject.Inject;

import org.junit.Assert;
import org.skyve.content.ContentManager;
import org.skyve.dataaccess.sql.SQLDataAccess;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

public class InjectedDocumentBizlet extends Bizlet<InjectedDocumentExtension> {
	private static final long serialVersionUID = -7149620934909349430L;

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
	Connection con;
	@Inject
	ContentManager cm;
	@Inject
	SQLDataAccess sda;
	
	
	@Override
	public InjectedDocumentExtension newInstance(InjectedDocumentExtension test) throws Exception {
		Assert.assertNotNull(test.p);
		Assert.assertNotNull(test.c);
		Assert.assertNotNull(test.u);
		Assert.assertNotNull(test.s);
		Assert.assertNotNull(test.r);
		Assert.assertNotNull(test.con);
		Assert.assertNotNull(test.cm);
		Assert.assertNotNull(test.sda);
		return test;
	}
}
