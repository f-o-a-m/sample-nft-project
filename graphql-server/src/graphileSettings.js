const { GRAPHQL_SIMPLE_AUTH_TOKEN } = require('./constants/environment')

const pullBearerTokenFromHeaders = ({ authorization }) => {
  if (!authorization) throw new Error('No auth header')
  const authParts = authorization.split(' ')
  if (authParts[0].toLowerCase() === 'bearer') return authParts[1]
  throw new Error('Invalid Auth header')
}

const validateToken = (token) => {
  if (token !== GRAPHQL_SIMPLE_AUTH_TOKEN) {
    throw new Error('Invalid Auth token')
  }
}

const setPgSettings = async (req) => {
  try {
    // const token = pullBearerTokenFromHeaders(req.headers)
    // validateToken(token)
    return {
      role: 'postgres',
    }
  } catch (err) {
    console.log('No Authentication: ', err.message)
    return {
      role: 'invalid_role',
    }
  }
}

module.exports = {
  setPgSettings,
}
