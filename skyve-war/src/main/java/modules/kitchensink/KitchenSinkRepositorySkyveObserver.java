package modules.kitchensink;

import org.skyve.CORE;
import org.skyve.impl.metadata.repository.DefaultRepository;
import org.skyve.impl.metadata.repository.LockableDynamicRepository;
import org.skyve.metadata.controller.Observer;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import jakarta.servlet.http.HttpSession;

public class KitchenSinkRepositorySkyveObserver implements Observer {
	public static final LockableDynamicRepository KITCHEN_SINK_REPOSITORY = new LockableDynamicRepository();
	
	private static final Logger LOGGER = LoggerFactory.getLogger(KitchenSinkRepositorySkyveObserver.class);

	@Override
	public void startup(Customer customer) {
		DefaultRepository r = (DefaultRepository) CORE.getRepository();
		r.addDelegate(0, KITCHEN_SINK_REPOSITORY);

	}

	@Override
	public void shutdown(Customer customer) {
		LOGGER.info("Shutting down....");

	}

	@Override
	public void beforeRestore(Customer customer) {
		LOGGER.info("Before Restore....");

	}

	@Override
	public void afterRestore(Customer customer) {
		LOGGER.info("After Restore....");

	}

	@Override
	public void login(User user, HttpSession session) {
		// TODO Auto-generated method stub

	}

	@Override
	public void logout(User user, HttpSession session) {
		// TODO Auto-generated method stub

	}

	@Override
	public void beforeBackup(Customer customer) {
		LOGGER.info("Before Backup....");
		
	}

	@Override
	public void afterBackup(Customer customer) {
		LOGGER.info("After Restore....");
		
	}
}
