package modules.test;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.Util;

import modules.test.domain.AnyDerived1;
import modules.test.domain.AnyDerived2;
import modules.test.domain.ArcOneToMany;
import modules.test.domain.ArcOneToOne;

/**
 * The arc tests fail as the class attributes of the any and many-to-any tags don't map correctly to
 * the entity-name s used in the current domain generation.
 * 
 * @author mike
 *
 */
public class ArcTests extends AbstractSkyveTest {
	/**
	 * This just wont work...
	 * 
	 * @throws Exception
	 */
	@Test(expected = DomainException.class)
	public void testOneToManyPersist() throws Exception {
		ArcOneToMany test = Util.constructRandomInstance(u, m, ao2m, 0);
		test = p.save(test);
		test.getArcs().add(p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0)));
		test.getArcs().add(p.save((AnyDerived2) Util.constructRandomInstance(u, m, ad2, 0)));
		test = p.save(test);

		p.evictAllCached();

		test = p.retrieve(ao2m, test.getBizId());
	}

	/**
	 * This works as long as the arc is saved first
	 * 
	 * @throws Exception
	 */
	@Test
	@Ignore // One to one arcs are screwed
	public void testOneToOnePersist() throws Exception {
		ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
		test = p.save(test);
		test.setArc(p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0)));
		test = p.save(test);

		p.evictAllCached();

		test = p.retrieve(ao2o, test.getBizId());
		Assert.assertNotNull(test.getArc());
	}

	/**
	 * This doesn't work as the arc was not saved first
	 * 
	 * @throws Exception
	 */
	@Test(expected = DomainException.class)
	public void testOneToOnePersistArcTransient() throws Exception {
		ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
		test = p.save(test);
		test.setArc((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0));
		test = p.save(test);

		p.evictAllCached();

		test = p.retrieve(ao2o, test.getBizId());
		Assert.assertNotNull(test.getArc());
	}

	/**
	 * This works when the arc is saved first
	 * 
	 * @throws Exception
	 */
	@Test
	@Ignore // One to one arcs are screwed
	public void testOneToOnePersistPartlyTransient() throws Exception {
		ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
		test.setArc(p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0)));
		test = p.save(test);

		p.evictAllCached();

		test = p.retrieve(ao2o, test.getBizId());
		Assert.assertNotNull(test.getArc());
	}

	/**
	 * This doesn't work when everything is transient - arc should be saved first
	 * 
	 * @throws Exception
	 */
	@Test(expected = DomainException.class)
	public void testOneToOnePersistFullyTransient() throws Exception {
		ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
		test.setArc((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0));
		test = p.save(test);
	}

	@Test
	public void testOneToOneUpsertInsert() throws Exception {
		ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
		test.setArc(p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0)));

		p.upsertBeanTuple(test);

		p.evictAllCached();

		test = p.retrieve(ao2o, test.getBizId());
		Assert.assertNotNull(test.getArc());
	}

	@Test
	@Ignore // One to one arcs are screwed
	public void testOneToOneUpsertUpdate() throws Exception {
		ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
		AnyDerived1 arc1 = p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0));
		AnyDerived1 arc2 = p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0));
		test.setArc(arc1);
		test = p.save(test);

		test.setArc(arc2);
		p.upsertBeanTuple(test);

		p.evictAllCached();

		test = p.retrieve(ao2o, test.getBizId());
		Assert.assertNotNull(test.getArc());
		Assert.assertEquals(arc2.getBizId(), test.getArc().getBizId());
	}
}
