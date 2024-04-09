package modules.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.DomainException;
import org.skyve.util.Util;

import modules.test.domain.AnyDerived1;
import modules.test.domain.AnyDerived2;
import modules.test.domain.ArcOneToMany;
import modules.test.domain.ArcOneToOne;

public class ArcTests extends AbstractSkyveTest {
	/**
	 * This just wont work...
	 * 
	 * @throws Exception
	 */
	@Test
	public void testOneToManyPersist() throws Exception {
		DomainException de = Assert.assertThrows(DomainException.class, () -> {
			ArcOneToMany test = Util.constructRandomInstance(u, m, ao2m, 0);
			test = p.save(test);
			test.getArcs().add(p.save((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0)));
			test.getArcs().add(p.save((AnyDerived2) Util.constructRandomInstance(u, m, ad2, 0)));
			test = p.save(test);

			p.evictAllCached();

			test = p.retrieve(ao2m, test.getBizId());
		});

		assertThat(de.getMessage(), is(notNullValue()));
	}

	/**
	 * This works as long as the arc is saved first
	 * 
	 * @throws Exception
	 */
	@Test
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
	@Test
	public void testOneToOnePersistArcTransient() throws Exception {
		DomainException de = Assert.assertThrows(DomainException.class, () -> {
			ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
			test = p.save(test);
			test.setArc((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0));
			test = p.save(test);

			p.evictAllCached();

			test = p.retrieve(ao2o, test.getBizId());
			Assert.assertNotNull(test.getArc());
		});

		assertThat(de.getMessage(), is(notNullValue()));
	}

	/**
	 * This works when the arc is saved first
	 * 
	 * @throws Exception
	 */
	@Test
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
	@Test
	public void testOneToOnePersistFullyTransient() throws Exception {
		DomainException de = Assert.assertThrows(DomainException.class, () -> {
			ArcOneToOne test = Util.constructRandomInstance(u, m, ao2o, 0);
			test.setArc((AnyDerived1) Util.constructRandomInstance(u, m, ad1, 0));
			test = p.save(test);
		});

		assertThat(de.getMessage(), is(notNullValue()));
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
