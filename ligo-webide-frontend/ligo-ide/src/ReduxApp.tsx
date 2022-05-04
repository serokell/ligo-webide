import '@/menu';

import { GlobalModals, autoUpdater } from '@obsidians/global';
import React, { Suspense, lazy, useState, useEffect, useRef } from 'react';
// import Welcome, { checkDependencies } from '@obsidians/welcome';
import { config, updateStore } from '@/redux';
import redux, { Provider } from '@obsidians/redux';

import Auth from '@obsidians/auth';
import { LoadingScreen } from '@obsidians/ui-components';
import { NotificationSystem } from '@obsidians/notification';
import Routes from './components/Routes';
import fileOps, { indexedDBFileSystem, fileSystems, fileSystem } from '@obsidians/file-ops';
import icon from './components/icon.png';

const Header = lazy(() =>
  import('./components/Header' /* webpackChunkName: "header" */)
);

const ReduxApp = (props) => {
  const [loaded, setLoaded] = useState(false)
  // const [dependencies, setDependencies] = useState(false)
  const remixFileSystems = useRef<fileSystems>(new fileSystems())
  const remixIndexedDB = useRef<fileSystem>(new indexedDBFileSystem())

  const refresh = async () => {
    // const dependencies = await checkDependencies();
    setLoaded(true)
    // setDependencies(dependencies)
    autoUpdater.check();
  };

  const skip = () => {
    setLoaded(true)
    // setDependencies(true)
  };

  useEffect(() => {
    async function loadStorage() {
      await redux.init(config, updateStore).then(onReduxLoaded);
      await remixFileSystems.current.addFileSystem(remixIndexedDB.current)
      await remixFileSystems.current.setFileSystem([remixIndexedDB.current])
      refresh();
    }
    loadStorage()
  }, [])

  if (!loaded) {
    return <LoadingScreen />;
  }

  // if (!dependencies) {
  //   return (
  //     <Suspense fallback={<LoadingScreen />}>
  //       {/* <Welcome
  //         isReady={checkDependencies}
  //         onGetStarted={skip}
  //         truffleSubtitle={`The library used to create and compile a project.`}
  //         enableTutorial={false}
  //       /> */}
  //       <NotificationSystem />
  //       <GlobalModals icon={icon} />
  //     </Suspense>
  //   );
  // }
  return (
    <Provider store={redux.store}>
      <div
        className="body"
        style={{ paddingTop: true ? '49px' : '0' }}
      >
        <Routes>
          <Header history={props.history} />
          <NotificationSystem />
          <GlobalModals icon={icon} />
        </Routes>
      </div>
    </Provider>
  );
}

export default ReduxApp

async function onReduxLoaded() {
  Auth.restore();
  const version = await fileOps.current.getAppVersion();
  redux.dispatch('SET_VERSION', { version });
}
