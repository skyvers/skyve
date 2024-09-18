package modules.admin.UserLoginRecord;

import org.skyve.util.test.SkyveFactory;

import modules.admin.domain.UserLoginRecord;

@SkyveFactory(excludedUpdateAttributes = {UserLoginRecord.countryCodePropertyName, UserLoginRecord.countryNamePropertyName})
public class UserLoginRecordFactory {
	// nothing to see here
}
