package org.bouncycastle.jce.provider;

import java.security.Permission;

import javax.crypto.spec.DHParameterSpec;

import org.bouncycastle.jcajce.provider.asymmetric.ec.EC5Util;
import org.bouncycastle.jcajce.provider.config.ConfigurableProvider;
import org.bouncycastle.jcajce.provider.config.ProviderConfiguration;
import org.bouncycastle.jcajce.provider.config.ProviderConfigurationPermission;
import org.bouncycastle.jce.spec.ECParameterSpec;

class BouncyCastleProviderConfiguration
    implements ProviderConfiguration
{
    private static Permission BC_EC_LOCAL_PERMISSION = new ProviderConfigurationPermission(
        BouncyCastleProvider.PROVIDER_NAME, ConfigurableProvider.THREAD_LOCAL_EC_IMPLICITLY_CA);
    private static Permission BC_EC_PERMISSION = new ProviderConfigurationPermission(
        BouncyCastleProvider.PROVIDER_NAME, ConfigurableProvider.EC_IMPLICITLY_CA);
    private static Permission BC_DH_LOCAL_PERMISSION = new ProviderConfigurationPermission(
        BouncyCastleProvider.PROVIDER_NAME, ConfigurableProvider.THREAD_LOCAL_DH_DEFAULT_PARAMS);
    private static Permission BC_DH_PERMISSION = new ProviderConfigurationPermission(
        BouncyCastleProvider.PROVIDER_NAME, ConfigurableProvider.DH_DEFAULT_PARAMS);

    private ThreadLocal ecThreadSpec = new ThreadLocal();
    private ThreadLocal dhThreadSpec = new ThreadLocal();

    private volatile ECParameterSpec ecImplicitCaParams;
    private volatile DHParameterSpec dhDefaultParams;

    void setParameter(String parameterName, Object parameter)
    {
        SecurityManager securityManager = System.getSecurityManager();

        if (parameterName.equals(ConfigurableProvider.THREAD_LOCAL_EC_IMPLICITLY_CA))
        {
            ECParameterSpec curveSpec;

            if (securityManager != null)
            {
                securityManager.checkPermission(BC_EC_LOCAL_PERMISSION);
            }

            if (parameter instanceof ECParameterSpec || parameter == null)
            {
                curveSpec = (ECParameterSpec)parameter;
            }
            else  // assume java.security.spec
            {
                curveSpec = EC5Util.convertSpec((java.security.spec.ECParameterSpec)parameter, false);
            }

            if (curveSpec == null)
            {
                ecThreadSpec.remove();
            }
            else
            {
                ecThreadSpec.set(curveSpec);
            }
        }
        else if (parameterName.equals(ConfigurableProvider.EC_IMPLICITLY_CA))
        {
            if (securityManager != null)
            {
                securityManager.checkPermission(BC_EC_PERMISSION);
            }

            if (parameter instanceof ECParameterSpec || parameter == null)
            {
                ecImplicitCaParams = (ECParameterSpec)parameter;
            }
            else  // assume java.security.spec
            {
                ecImplicitCaParams = EC5Util.convertSpec((java.security.spec.ECParameterSpec)parameter, false);
            }
        }
        else if (parameterName.equals(ConfigurableProvider.THREAD_LOCAL_DH_DEFAULT_PARAMS))
        {
            DHParameterSpec dhSpec;

            if (securityManager != null)
            {
                securityManager.checkPermission(BC_DH_LOCAL_PERMISSION);
            }

            if (parameter instanceof DHParameterSpec || parameter == null)
            {
                dhSpec = (DHParameterSpec)parameter;
            }
            else
            {
                throw new IllegalArgumentException("not a valid DHParameterSpec");
            }

            if (dhSpec == null)
            {
                dhThreadSpec.remove();
            }
            else
            {
                dhThreadSpec.set(dhSpec);
            }
        }
        else if (parameterName.equals(ConfigurableProvider.DH_DEFAULT_PARAMS))
        {
            if (securityManager != null)
            {
                securityManager.checkPermission(BC_DH_PERMISSION);
            }

            if (parameter instanceof DHParameterSpec || parameter == null)
            {
                dhDefaultParams = (DHParameterSpec)parameter;
            }
            else
            {
                throw new IllegalArgumentException("not a valid DHParameterSpec");
            }
        }
    }

    public ECParameterSpec getEcImplicitlyCa()
    {
        ECParameterSpec spec = (ECParameterSpec)ecThreadSpec.get();

        if (spec != null)
        {
            return spec;
        }

        return ecImplicitCaParams;
    }

    public DHParameterSpec getDHDefaultParameters()
    {
        DHParameterSpec spec = (DHParameterSpec)dhThreadSpec.get();

        if (spec != null)
        {
            return spec;
        }

        return dhDefaultParams;
    }
}
