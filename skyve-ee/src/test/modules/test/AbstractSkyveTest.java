package modules.test;

import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import modules.test.domain.AllAttributesInverseOneToOnePersistent;
import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.AnyDerived1;
import modules.test.domain.AnyDerived2;
import modules.test.domain.ArcOneToMany;
import modules.test.domain.ArcOneToOne;
import modules.test.domain.Hierarchical;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;
import modules.test.domain.UniqueConstraintNonNullable;
import modules.test.domain.UniqueConstraintNullable;
import util.AbstractH2Test;

public abstract class AbstractSkyveTest extends AbstractH2Test {

	protected User u;
	protected Customer c;
	protected Module m;
	protected Document aapd;
	protected Document aai121pd;
	protected Document aarpd;
	protected Document ad1;
	protected Document ad2;
	protected Document ao2m;
	protected Document ao2o;
	protected Document hd;
	protected Document mbd;
	protected Document mejsd;
	protected Document messd;
	protected Document msjsd;
	protected Document msssd;
	protected Document ucn;
	protected Document ucnn;

	@Override
	public void beforeBase() {
		super.beforeBase();

		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		aapd = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		aai121pd = m.getDocument(c, AllAttributesInverseOneToOnePersistent.DOCUMENT_NAME);
		aarpd = m.getDocument(c, AllAttributesRequiredPersistent.DOCUMENT_NAME);
		ad1 = m.getDocument(c, AnyDerived1.DOCUMENT_NAME);
		ad2 = m.getDocument(c, AnyDerived2.DOCUMENT_NAME);
		ao2m = m.getDocument(c, ArcOneToMany.DOCUMENT_NAME);
		ao2o = m.getDocument(c, ArcOneToOne.DOCUMENT_NAME);
		hd = m.getDocument(c, Hierarchical.DOCUMENT_NAME);
		mbd = m.getDocument(c, MappedBase.DOCUMENT_NAME);
		mejsd = m.getDocument(c, MappedExtensionJoinedStrategy.DOCUMENT_NAME);
		messd = m.getDocument(c, MappedExtensionSingleStrategy.DOCUMENT_NAME);
		msjsd = m.getDocument(c, MappedSubclassedJoinedStrategy.DOCUMENT_NAME);
		msssd = m.getDocument(c, MappedSubclassedSingleStrategy.DOCUMENT_NAME);
		ucn = m.getDocument(c, UniqueConstraintNullable.DOCUMENT_NAME);
		ucnn = m.getDocument(c, UniqueConstraintNonNullable.DOCUMENT_NAME);
	}
}
