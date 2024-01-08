package modules.test;

import java.util.UUID;

import org.junit.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.impl.domain.AbstractPersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.user.User;
import org.skyve.persistence.Persistence;
import org.skyve.util.PushMessage;
import org.skyve.util.Util;

import modules.test.domain.AllAttributesPersistent;
import modules.test.domain.UniqueConstraintNonNullable;
import modules.test.domain.UniqueConstraintNonNullable.Enum3;

public class DatabaseIsolationTests extends AbstractSkyveTestDispose {
	// insert a duplicate where the duplicate exists in the database
	@Test(expected = DomainException.class)
	public void testCommittedDuplicateDetectedOnStatement() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		((AbstractPersistentBean) test1).setBizId(test.getBizId());
		p.upsertBeanTuple(test);
		p.commit(false);
		p.upsertBeanTuple(test1);
	}

	// insert a duplicate where the duplicate was first inserted in this transaction
	@Test(expected = DomainException.class)
	public void testUncommittedDuplicateDetectedInSameTransactionOnStatement() throws Exception {
		AllAttributesPersistent test = Util.constructRandomInstance(u, m, aapd, 1);
		AllAttributesPersistent test1 = Util.constructRandomInstance(u, m, aapd, 1);
		((AbstractPersistentBean) test1).setBizId(test.getBizId());
		p.upsertBeanTuple(test);
		p.commit(false);
		p.upsertBeanTuple(test1);
	}

	// insert a duplicate where the duplicate was first inserted in another transaction
	// NB Run this as a job in Skyve - add "test" module to "demo" customer
	
	private static class SaveThread extends Thread {
		private User user;
		private String text;
		
		public SaveThread(User user, String text) {
			this.user = user;
			this.text = text;
		}

		@Override
		public void run() {
			//long millis = System.currentTimeMillis();
			AbstractPersistence p1 = AbstractPersistence.get();
			p1.setUser(user);
			try {
				//System.out.println("Insert text = " + text);
				UniqueConstraintNonNullable test = UniqueConstraintNonNullable.newInstance();
				test.setBooleanFlag(Boolean.FALSE);
				test.setEnum3(Enum3.one);
				test.setText(text);
				p1.begin();
				p1.save(test);
				//System.out.println("millis = " + (System.currentTimeMillis() - millis));
			}
			catch (@SuppressWarnings("unused") Exception e) {
				//e.printStackTrace();
				p1.rollback();
			}
			finally {
				p1.commit(true);
			}
		}
	}

	@SuppressWarnings("static-method") // will be overridden in a Job
	public void execute() throws Exception {
		EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info,
				"Generate Test Data Job has been started"));
		for (int i = 0, l = 1000; i < l; i++) {
			System.out.println("**************BATCH=" + i);
			test();
		}
		EXT.push(new PushMessage().user(CORE.getUser()).growl(MessageSeverity.info, "Finito"));
	}

	private static void test() throws InterruptedException {
		Persistence p = CORE.getPersistence();
		User u = p.getUser();
		
		p.newSQL("delete from TEST_UniqueConstraintNonNullable").execute();
		p.newSQL("delete from ADM_Uniqueness").execute();
		p.commit(false);
		p.begin();
		
		int iterations = 1000;
		for (int i = 0; i < iterations; i++) {
			String text = UUID.randomUUID().toString();
			int sleep = (int) (Math.random() * 100.0);
			System.out.println("Iteration " + i + " text = " + text + " : sleep " + sleep);
			new SaveThread(u, text).start();
			Thread.sleep(sleep);
			new SaveThread(u, text).start();
		}
		Thread.sleep(80000);
		Number count = p.newSQL("select count(1) from TEST_UniqueConstraintNonNullable").scalarResult(Number.class);
		System.out.println("Data count = " + count);
		if (count.intValue() != iterations) {
			throw new DomainException("Should be " + iterations + " rows committed in database");
		}
		if (p.newSQL("select text from TEST_UniqueConstraintNonNullable group by text having count(text) > 1").scalarResult(String.class) != null) {
			throw new DomainException("Duplicates in database");
		}
		count = p.newSQL("select count(1) from ADM_Uniqueness").scalarResult(Number.class);
		System.out.println("Uniqueness count = " + count);
		if ((count != null) && (count.intValue() > 0)) {
			throw new DomainException("Uniqueness rows committed in database");
		}
	}
}
