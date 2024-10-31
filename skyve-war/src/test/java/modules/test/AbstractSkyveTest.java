package modules.test;

import org.junit.jupiter.api.BeforeEach;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.AllAttributesRequiredPersistent;
import modules.test.domain.AllDynamicAttributesPersistent;
import modules.test.domain.AnyDerived1;
import modules.test.domain.AnyDerived2;
import modules.test.domain.ArcOneToMany;
import modules.test.domain.ArcOneToOne;
import modules.test.domain.Hierarchical;
import modules.test.domain.InjectedDocument;
import modules.test.domain.InverseManyToManyPersistent;
import modules.test.domain.InverseOneToManyPersistent;
import modules.test.domain.InverseOneToOnePersistent;
import modules.test.domain.MappedBase;
import modules.test.domain.MappedExtensionJoinedStrategy;
import modules.test.domain.MappedExtensionSingleStrategy;
import modules.test.domain.MappedSubclassedJoinedStrategy;
import modules.test.domain.MappedSubclassedSingleStrategy;
import modules.test.domain.Reachability;
import modules.test.domain.UniqueConstraintMultipleNavigable;
import modules.test.domain.UniqueConstraintNonNullable;
import modules.test.domain.UniqueConstraintNullable;
import modules.test.domain.UniqueConstraintOptimisation;
import util.AbstractH2TestTruncate;

public abstract class AbstractSkyveTest extends AbstractH2TestTruncate {

	public static final String ALL_ATTRIBUTES_DYNAMIC_PERSISTENT_DOCUMENT_NAME = "AllAttributesDynamicPersistent";
	public static final String DYNAMIC_MAPPED_EXTENSION_DOCUMENT_NAME = "DynamicMappedExtension";
	public static final String DYNAMIC_MAPPED_SUBCLASSED_DOCUMENT_NAME = "DynamicMappedSubclassed";
	
	protected User u;
	protected Customer c;
	protected Module m;
	protected Document aapd;
	protected Document adapd;
	protected Document aadpd;
	protected Document aarpd;
	protected Document ad1;
	protected Document ad2;
	protected Document ao2m;
	protected Document ao2o;
	protected Document dmed;
	protected Document dmsd;
	protected Document hd;
	protected Document id;
	protected Document im2mpd;
	protected Document io2mpd;
	protected Document io2opd;
	protected Document mbd;
	protected Document mejsd;
	protected Document messd;
	protected Document msjsd;
	protected Document msssd;
	protected Document rd;
	protected Document ucno;
	protected Document ucn;
	protected Document ucnn;
	protected Document ucmn;

	protected Persistence p;

	@BeforeEach
	public void before() {
		p = CORE.getPersistence();
		u = p.getUser();
		c = u.getCustomer();
		m = c.getModule(AllAttributesPersistent.MODULE_NAME);
		aapd = m.getDocument(c, AllAttributesPersistent.DOCUMENT_NAME);
		adapd = m.getDocument(c, AllDynamicAttributesPersistent.DOCUMENT_NAME);
		aadpd = m.getDocument(c, ALL_ATTRIBUTES_DYNAMIC_PERSISTENT_DOCUMENT_NAME);
		aarpd = m.getDocument(c, AllAttributesRequiredPersistent.DOCUMENT_NAME);
		ad1 = m.getDocument(c, AnyDerived1.DOCUMENT_NAME);
		ad2 = m.getDocument(c, AnyDerived2.DOCUMENT_NAME);
		ao2m = m.getDocument(c, ArcOneToMany.DOCUMENT_NAME);
		ao2o = m.getDocument(c, ArcOneToOne.DOCUMENT_NAME);
		dmed = m.getDocument(c, DYNAMIC_MAPPED_EXTENSION_DOCUMENT_NAME);
		dmsd = m.getDocument(c, DYNAMIC_MAPPED_SUBCLASSED_DOCUMENT_NAME);
		hd = m.getDocument(c, Hierarchical.DOCUMENT_NAME);
		id = m.getDocument(c, InjectedDocument.DOCUMENT_NAME);
		im2mpd = m.getDocument(c, InverseManyToManyPersistent.DOCUMENT_NAME);
		io2mpd = m.getDocument(c, InverseOneToManyPersistent.DOCUMENT_NAME);
		io2opd = m.getDocument(c, InverseOneToOnePersistent.DOCUMENT_NAME);
		mbd = m.getDocument(c, MappedBase.DOCUMENT_NAME);
		mejsd = m.getDocument(c, MappedExtensionJoinedStrategy.DOCUMENT_NAME);
		messd = m.getDocument(c, MappedExtensionSingleStrategy.DOCUMENT_NAME);
		msjsd = m.getDocument(c, MappedSubclassedJoinedStrategy.DOCUMENT_NAME);
		msssd = m.getDocument(c, MappedSubclassedSingleStrategy.DOCUMENT_NAME);
		rd = m.getDocument(c, Reachability.DOCUMENT_NAME);
		ucno = m.getDocument(c, UniqueConstraintOptimisation.DOCUMENT_NAME);
		ucn = m.getDocument(c, UniqueConstraintNullable.DOCUMENT_NAME);
		ucnn = m.getDocument(c, UniqueConstraintNonNullable.DOCUMENT_NAME);
		ucmn = m.getDocument(c, UniqueConstraintMultipleNavigable.DOCUMENT_NAME);
	}
}
